package core;

import ij.IJ;
import ij.ImagePlus;
import ij.ImageStack;
import ij.gui.Roi;
import ij.process.ByteProcessor;
import ij.process.ImageProcessor;

import java.awt.*;
import java.io.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This code has been taken from the original file ParticleTracker.java<br />
 * It could not be shared as we had no rights to change the original code so we took just what we needed for the
 * batch program.
 * <p>
 *   Generates the trajectories from a given set of image files. Uses <strong>ImageJ</strong> to analyse the
 *   image frames
 * </p>
 *
 *
 * User: zola
 * Date: 16/04/2013
 */
public class ParticleTrackerOriginal {

  private static final Logger LOGGER = Logger.getLogger("global");

  private boolean frames_processed;
  public MyFrame[] frames;
  boolean text_files_mode = true;
  ImageStack trajectoryStack;

  /* user defined parameters */
  public double cutoff = Double.valueOf(System.getProperty("particle.cutoff.radius"));
  public float percentile = Float.valueOf(System.getProperty("particle.percentile"));
  public int radius = Integer.valueOf(System.getProperty("particle.kernal.radius"));
  public int linkRange = Integer.valueOf(System.getProperty("particle.linkrange"));
  public double displacement = Double.valueOf(System.getProperty("particle.displacement"));
  private int trajectoryTail;

  private ImagePlus originalImp;
  int maxCoord = 0;
  boolean momentum_from_text;

  public int numTrajectories, frameNum;


  public ArrayList<TrajectoryOriginal> allTraj;
  private File mDirectory;


  public ParticleTrackerOriginal(final File directory) {
    mDirectory = directory;
  }


  /**
   * This method runs the plugin, what implemented here is what the plugin actually
   * does. It takes the image processor it works on as an argument.
   * <br>In this implementation the processor is not used so that the original image is left unchanged.
   * <br>The original image is locked while the plugin is running.
   * <br>This method is called by ImageJ after <code>setup(String arg, ImagePlus imp)</code> returns
   */
  public boolean run(final File outputFile, final boolean includeHeaders) {

    File[] inputFiles = mDirectory.listFiles();

  		/* get user defined params and set more initial params accordingly 	*/
    if (!getUserDefinedParams()) return false;

    if (!processFrames(inputFiles)) return false;

    LOGGER.info(getConfiguration().toString());

    if (text_files_mode) {
        /* create an ImagePlus object to hold the particle information from the text files*/
      originalImp = new ImagePlus("From text files", createStackFromTextFiles());
    }

  		/* link the particles found */

    LOGGER.info("Linking particles");
    linkParticles();

    LOGGER.info("Generating trajectories");
    generateTrajectories();

    boolean successful = writeReportUsingIO(outputFile, includeHeaders);

    LOGGER.info(String.format("Generated report with success=%s to file=%s", successful, outputFile.getAbsolutePath()));
    return successful;
  }

  /**
   * This is a really slow way to write to the filesystem but the requirement was to use Java 1.6,
   * if you change to Java 1.7 and above its strongly recommended you change to using the new NIO package.
   *
   * @return true if it was successful
   */
  private boolean writeReportUsingIO(final File outputFile, final boolean includeHeaders) {
    boolean successful = true;
    FileWriter writer;
    try {

      writer = new FileWriter(outputFile);
      writer.write(getTrajectoryCSVReport(includeHeaders));
      writer.flush();
      writer.close();

    } catch (FileNotFoundException e) {
      successful = false;
      System.err.println("FileStreamsReadnWrite: " + e);
    } catch (IOException e) {
      successful = false;

      System.err.println("Failed to write the report " + e);
    }

    return successful;
  }


  public boolean processFrames(final File[] inputFiles) {

    if (frames_processed) return true;

    frameNum = inputFiles.length;

  		/* Initialise frames array */
    frames = new MyFrame[inputFiles.length];
    MyFrame currentFrame;

    for (File inputFile : inputFiles) {
      if (inputFile.getName().startsWith(".") || inputFile.getName().endsWith("~")) {
        continue;
      }

      LOGGER.log(Level.INFO, "Reading Particles from file " + inputFile.getName());

      currentFrame = new MyFrame(inputFile);
      if (currentFrame.particles == null) return false;
      frames[currentFrame.frame_number] = currentFrame;
    }


    frames_processed = true;
    return true;
  }

  /**
   * Creates a new <code>ImageStack</code> and draws particles on it according to
   * the particle positions defined in <code>frames</code>.
   * <br>It is used to visualize particles and trajectories when working in text-files-mode,
   * since there is no visual stack to start with
   *
   * @return the created ImageStack
   *         //   * @see MyFrame#createImage(int, int)
   */
  public ImageStack createStackFromTextFiles() {

 		/* Create a new, empty, square ImageStack with 10 pixels padding from the max particle position*/
    ImageStack from_text = new ImageStack(maxCoord + 10, maxCoord + 10);

 		/* for each frame we have add a slice (ImageProcessor) to the stack*/
    for (int i = 0; i < frames.length; i++) {
      from_text.addSlice("" + i, frames[i].createImage(maxCoord + 10, maxCoord + 10));
    }
    return from_text;
  }

  /**
   * Second phase of the algorithm -
   * <br>Identifies points corresponding to the
   * same physical particle in subsequent frames and links the positions into trajectories
   * <br>The length of the particles next array will be reset here according to the current linkRange
   * <br>Adapted from Ingo Oppermann implementation
   */
  private void linkParticles() {

    int m, i, j, k, nop, nop_next, n;
    int ok, prev, prev_s, x = 0, y = 0, curr_linkrange;
    int[] g;
    double min, z, max_cost;
    double[] cost;
    Particle[] p1, p2;

    // set the length of the particles next array according to the linkRange
    // it is done now since link range can be modified after first run
    for (int fr = 0; fr < frames.length; fr++) {
      for (int pr = 0; pr < frames[fr].particles.length; pr++) {
        frames[fr].particles[pr].next = new int[linkRange];
      }
    }
    curr_linkrange = this.linkRange;

     	/* If the linkRange is too big, set it the right value */
    if (frameNum < (curr_linkrange + 1))
      curr_linkrange = frameNum - 1;

    max_cost = this.displacement * this.displacement;

    for (m = 0; m < frameNum - curr_linkrange; m++) {
      nop = frames[m].particles.length;
      for (i = 0; i < nop; i++) {
        frames[m].particles[i].special = false;
        for (n = 0; n < this.linkRange; n++)
          frames[m].particles[i].next[n] = -1;
      }

      for (n = 0; n < curr_linkrange; n++) {
        max_cost = (double) (n + 1) * this.displacement * (double) (n + 1) * this.displacement;

        nop_next = frames[m + (n + 1)].particles.length;

     			/* Set up the cost matrix */
        cost = new double[(nop + 1) * (nop_next + 1)];

     			/* Set up the relation matrix */
        g = new int[(nop + 1) * (nop_next + 1)];

     			/* Set g to zero */
        for (i = 0; i < g.length; i++) g[i] = 0;

        p1 = frames[m].particles;
        p2 = frames[m + (n + 1)].particles;

     			/* Fill in the costs */
        for (i = 0; i < nop; i++) {
          for (j = 0; j < nop_next; j++) {
            cost[coord(i, j, nop_next + 1)] = (p1[i].x - p2[j].x) * (p1[i].x - p2[j].x) +
                (p1[i].y - p2[j].y) * (p1[i].y - p2[j].y) +
                (p1[i].m0 - p2[j].m0) * (p1[i].m0 - p2[j].m0) +
                (p1[i].m2 - p2[j].m2) * (p1[i].m2 - p2[j].m2);
          }
        }

        for (i = 0; i < nop + 1; i++)
          cost[coord(i, nop_next, nop_next + 1)] = max_cost;
        for (j = 0; j < nop_next + 1; j++)
          cost[coord(nop, j, nop_next + 1)] = max_cost;
        cost[coord(nop, nop_next, nop_next + 1)] = 0.0;

     			/* Initialize the relation matrix */
        for (i = 0; i < nop; i++) { // Loop over the x-axis
          min = max_cost;
          prev = 0;
          for (j = 0; j < nop_next; j++) { // Loop over the y-axis               /* Let's see if we can use this coordinate */
            ok = 1;
            for (k = 0; k < nop + 1; k++) {
              if (g[coord(k, j, nop_next + 1)] == 1) {
                ok = 0;
                break;
              }
            }
            if (ok == 0) // No, we can't. Try the next column
              continue;

     					/* This coordinate is OK */
            if (cost[coord(i, j, nop_next + 1)] < min) {
              min = cost[coord(i, j, nop_next + 1)];
              g[coord(i, prev, nop_next + 1)] = 0;
              prev = j;
              g[coord(i, prev, nop_next + 1)] = 1;
            }
          }

     				/* Check if we have a dummy particle */
          if (min == max_cost) {
            g[coord(i, prev, nop_next + 1)] = 0;
            g[coord(i, nop_next, nop_next + 1)] = 1;
          }
        }

     			/* Look for columns that are zero */
        for (j = 0; j < nop_next; j++) {
          ok = 1;
          for (i = 0; i < nop + 1; i++) {
            if (g[coord(i, j, nop_next + 1)] == 1)
              ok = 0;
          }

          if (ok == 1)
            g[coord(nop, j, nop_next + 1)] = 1;
        }

     			/* The relation matrix is initilized */

     			/* Now the relation matrix needs to be optimized */
        min = -1.0;
        while (min < 0.0) {
          min = 0.0;
          prev = 0;
          prev_s = 0;
          for (i = 0; i < nop + 1; i++) {
            for (j = 0; j < nop_next + 1; j++) {
              if (i == nop && j == nop_next)
                continue;

              if (g[coord(i, j, nop_next + 1)] == 0 &&
                  cost[coord(i, j, nop_next + 1)] <= max_cost) {                   /* Calculate the reduced cost */

                // Look along the x-axis, including
                // the dummy particles
                for (k = 0; k < nop + 1; k++) {
                  if (g[coord(k, j, nop_next + 1)] == 1) {
                    x = k;
                    break;
                  }
                }

                // Look along the y-axis, including
                // the dummy particles
                for (k = 0; k < nop_next + 1; k++) {
                  if (g[coord(i, k, nop_next + 1)] == 1) {
                    y = k;
                    break;
                  }
                }

     							/* z is the reduced cost */
                if (j == nop_next)
                  x = nop;
                if (i == nop)
                  y = nop_next;

                z = cost[coord(i, j, nop_next + 1)] +
                    cost[coord(x, y, nop_next + 1)] -
                    cost[coord(i, y, nop_next + 1)] -
                    cost[coord(x, j, nop_next + 1)];
                if (z > -1.0e-10)
                  z = 0.0;
                if (z < min) {
                  min = z;
                  prev = coord(i, j, nop_next + 1);
                  prev_s = coord(x, y, nop_next + 1);
                }
              }
            }
          }

          if (min < 0.0) {
            g[prev] = 1;
            g[prev_s] = 1;
            g[coord(prev / (nop_next + 1), prev_s % (nop_next + 1), nop_next + 1)] = 0;
            g[coord(prev_s / (nop_next + 1), prev % (nop_next + 1), nop_next + 1)] = 0;
          }
        }

     			/* After optimization, the particles needs to be linked */
        for (i = 0; i < nop; i++) {
          for (j = 0; j < nop_next; j++) {
            if (g[coord(i, j, nop_next + 1)] == 1)
              p1[i].next[n] = j;
          }
        }
      }

      if (m == (frameNum - curr_linkrange - 1) && curr_linkrange > 1)
        curr_linkrange--;
    }

     	/* At the last frame all trajectories end */
    for (i = 0; i < frames[frameNum - 1].particles.length; i++) {
      frames[frameNum - 1].particles[i].special = false;
      for (n = 0; n < this.linkRange; n++)
        frames[frameNum - 1].particles[i].next[n] = -1;
    }


  }

  private int coord(int a, int b, int c) {
    return (((a) * (c)) + (b));
  }

  /**
   * Displays a dialog window to get user defined params and selections,
   * also initialize and sets other params according to the work mode.
   * <ul>
   * <br>For a sequence of images:
   * <ul>
   * <li>Gets user defined params:<code> radius, cutoff, precentile, linkRange, displacement</code>
   * <li>Displays the preview Button and slider
   * <li>Gives the option to convert the image seq to 8Bit if its color
   * <li>Initialize and sets params:<code> stack, title, globalMax, globalMin, mask, kernel</code>
   * <br></ul>
   * For text_files_mode:
   * <ul>
   * <li>Gets user defined params:<code> linkRange, displacement </code>
   * <li>Initialize and sets params:<code> files_list, title, frameNum, momentum_from_text </code>
   * </ul></ul>
   *
   * @return false if cancel button clicked or problem with input
   *         //   * @see #makeKernel(int)
   *         //   * @see #generateMask(int)
   */
  boolean getUserDefinedParams() {
    return true;
  }


  /**
   * Generates (in real time) a "ready to print" report with information
   * about the user defined parameters:
   * <ul>
   * <li> Radius
   * <li> Cutoff
   * <li> Percentile
   * <li> Displacement
   * <li> Linkrange
   * </ul>
   *
   * @return a <code>StringBuffer</code> that holds this information
   */
  private StringBuffer getConfiguration() {

    StringBuffer configuration = new StringBuffer("% Configuration:\n");
    if (!this.text_files_mode) {
      configuration.append("% \tKernel radius: ");
      configuration.append(this.radius);
      configuration.append("\n");
      configuration.append("% \tCutoff radius: ");
      configuration.append(this.cutoff);
      configuration.append("\n");
      configuration.append("% \tPercentile   : ");
      configuration.append((this.percentile * 100));
      configuration.append("\n");
    }
    configuration.append("% \tKernel radius: ");
    configuration.append(this.radius);
    configuration.append("\n");
    configuration.append("% \tCutoff radius: ");
    configuration.append(this.cutoff);
    configuration.append("\n");
    configuration.append("% \tPercentile   : ");
    configuration.append((this.percentile * 100));
    configuration.append("\n");
    configuration.append("% \tDisplacement : ");
    configuration.append(this.displacement);
    configuration.append("\n");
    configuration.append("% \tLinkrange    : ");
    configuration.append(this.linkRange);
    configuration.append("\n");
    return configuration;
  }

  public String getTrajectoryCSVReport(final boolean includeHeaders) {
    StringBuilder report = new StringBuilder();
    Iterator iter = allTraj.iterator();

    if (includeHeaders) {
      report.append("traj,frame,x,y\n");
    }

    while (iter.hasNext()) {
      TrajectoryOriginal currentTraj = (TrajectoryOriginal) iter.next();
      java.util.List<String> lines = new ArrayList<String>();
      lines.addAll(currentTraj.toCSV(currentTraj.serial_number));
      for (String line : lines) {
        report.append(line);
        report.append("\n");
      }
    }

    return report.toString();

  }

  public ArrayList<TrajectoryOriginal> getAllTraj(){
    return allTraj;
  }


  /**
   * Generates <code>Trajectory</code> objects according to the infoamtion
   * avalible in each MyFrame and Particle.
   * <br>Populates the <code>allTraj</code> Vector.
   */
  private void generateTrajectories() {

    int i, j, k;
    int found, n, m;
    // Bank of colors from which the trjectories color will be selected
    Color[] col = {Color.blue, Color.green, Color.orange, Color.cyan, Color.magenta, Color.yellow, Color.white, Color.gray, Color.pink};

    TrajectoryOriginal currTraj;
    // temporary vector to hold particles for current trajctory
    java.util.List<Particle> currTrajParticles = new ArrayList<Particle>(frameNum);
    // initialize trajectories vector
    allTraj = new ArrayList<TrajectoryOriginal>();
    this.numTrajectories = 0;

    for (i = 0; i < frameNum; i++) {
      for (j = 0; j < this.frames[i].particles.length; j++) {
        if (!this.frames[i].particles[j].special) {
          this.frames[i].particles[j].special = true;
          found = -1;
          // go over all particles that this particle (particles[j]) is linked to
          for (n = 0; n < this.linkRange; n++) {
            // if it is NOT a dummy particle - stop looking
            if (this.frames[i].particles[j].next[n] != -1) {
              found = n;
              break;
            }
          }
          // if this particle is not linked to any other
          // go to next particle and dont add a trajectory
          if (found == -1)
            continue;

          // Added by Guy Levy, 18.08.06 - A change form original implementation
          // if this particle is linkd to a "real" paritcle that was already linked
          // break the trajectory and start again from the next particle. dont add a trajectory
          if (this.frames[i + n + 1].particles[this.frames[i].particles[j].next[n]].special)
            continue;

          // this particle is linked to another "real" particle that is not already linked
          // so we have a trajectory
          this.numTrajectories++;
          currTrajParticles.add(this.frames[i].particles[j]);
          k = i;
          m = j;
          do {
            found = -1;
            for (n = 0; n < this.linkRange; n++) {
              if (this.frames[k].particles[m].next[n] != -1) {
                // If this particle is linked to a "real" particle that
                // that is NOT already linked, continue with building the trajectory
                if (this.frames[k + n + 1].particles[this.frames[k].particles[m].next[n]].special == false) {
                  found = n;
                  break;
                  // Added by Guy Levy, 18.08.06 - A change form original implementation
                  // If this particle is linked to a "real" particle that
                  // that is already linked, stop building the trajectory
                } else {
                  break;
                }
              }
            }
            if (found == -1)
              break;
            m = this.frames[k].particles[m].next[found];
            k += (found + 1);
            currTrajParticles.add(this.frames[k].particles[m]);
            this.frames[k].particles[m].special = true;
          } while (m != -1);

          // Create the current trajectory
          Particle[] currTrajParticlesArray = new Particle[currTrajParticles.size()];
          currTraj = new TrajectoryOriginal(currTrajParticles.toArray(currTrajParticlesArray));

          // set current trajectory parameters
          currTraj.serial_number = this.numTrajectories;
          currTraj.color = col[this.numTrajectories % col.length];
          currTraj.setFocusArea();
          currTraj.setMouseSelectionArea();
          currTraj.populateGaps();
          // add current trajectory to allTraj vactor
          allTraj.add(currTraj);
          // clear temporary vector
          currTrajParticles.clear();
        }
      }
    }
  }


  /**
   * Defines a MyFrame that is based upon an ImageProcessor or information from a text file.
   * <br>MyFrame class has all the necessary methods to detect and report the "real" particles
   * for them to be linked.
   * <br>Some of its methods use global variables defined and calculated in <code>ParticleTracker_</code>
   * <p/>
   * //   * @see ParticleTracker_#mask
   * //   * @see ParticleTracker_#kernel
   * //   * @see ParticleTracker_#cutoff
   * //   * @see ParticleTracker_#percentile
   * //   * @see ParticleTracker_#radius
   * //   * @see ParticleTracker_#linkRange
   * //   * @see ParticleTracker_#globalMax
   * //   * @see ParticleTracker_#globalMin
   */
  public class MyFrame {

    Particle[] particles;    // an array Particle, holds all the particles detected in this frame
    int particleNum;    // number of particles initialy detected
    int frame_number;      // Serial number of this frame in the movie (can be 0)

    /**
     * Constructor for <code>text_files_mode</code>.
     * <br>constructs a MyFrame from a text file that holds the frame number and
     * particles information. unlike the <code>ImageProcessor</code> based constructor,
     * all the particles information is set immediately on construction.
     * <p/>
     * //     * @param path full path to the file (including full file name) e.g c:\ImageJ\frame0.txt
     */
    public MyFrame(final File inputFile) {
      loadParticlesFromFile(inputFile);
    }

    /**
     * ONLY FOR text_files_mode.
     * <br>Loads particles information for this frame from the file located
     * at the given path and adds these particles to the <code>particles</code> array.
     * <br>These particles are considered to be "after discrimination".
     * <br>File must have the word 'frame' (case sensitive) at the beginning of the first line
     * followed by any number of space characters (\t \n) and the frame number.
     * <br>Each next line represents a particle in the frame number given at the first line.
     * <br>Each line must have 2 numbers or more separated by one or more space characters.
     * <br>The 2 first numbers represents the X and Y coordinates of the particle (respectfully).
     * <br>The next numbers represent other information of value about the particle
     * (this information can be plotted later along a trajectory).
     * <br>The number of parameters must be equal for all particles.
     * <br>For more about X and Y coordinates (they are not in the usual graph coord) see <code>Particle</code>
     * <p/>
     * //     * @param path full path to the file (including full file name) e.g c:\ImageJ\frame0.txt
     *
     * @return false if there was any problem
     * @see Particle
     */
    private boolean loadParticlesFromFile(final File file) {

      java.util.List<String[]> particlesInfo = new ArrayList<String[]>();   // a vector to hold all particles info as String[]
      String[] particleInfo;         // will hold all the info for one particle (splitted)
      String[] frameInfoNum;        // will fold the frame info line (splitted)
      String line;

      try {                /* open the file */
        BufferedReader r = new BufferedReader(new FileReader(file));

  	            /* set this frame number from the first line*/
        line = r.readLine();
        if (line == null || !line.startsWith("frame")) {
          LOGGER.log(Level.SEVERE, "File: " + file.getName() + "\ndoesnt have the string 'frame' in the beginning if the first line");
        }
        line = line.trim();
        frameInfoNum = line.split("\\s+");
        if (frameInfoNum[1] != null) {
          this.frame_number = Integer.parseInt(frameInfoNum[1]);
        }

  		        /* go over all lines, count number of particles and save the information as String */
        while (true) {
          line = r.readLine();
          if (line == null) break;
          line = line.trim();
          if (line.startsWith("%")) line = line.substring(1);
          line = line.trim();
          particlesInfo.add(line.split("\\s+"));
          this.particleNum++;
        }                /* close file */
        r.close();
      } catch (Exception e) {
        LOGGER.log(Level.SEVERE, e.getMessage());
        return false;
      }

  	        /* initialise the particles array */
      this.particles = new Particle[this.particleNum];

      Iterator iter = particlesInfo.iterator();
      int counter = 0;

  	        /* go over all particles String info and construct Particles Ojectes from it*/
      while (iter.hasNext()) {
        particleInfo = (String[]) iter.next();
        this.particles[counter] = new Particle(Float.parseFloat(particleInfo[0]), Float.parseFloat(particleInfo[1]), this.frame_number, particleInfo);
        maxCoord = Math.max((int) Math.max(this.particles[counter].x, this.particles[counter].y), maxCoord);
        if (momentum_from_text) {
          if (particleInfo.length < 4 || particleInfo[2] == null || particleInfo[3] == null) {
            LOGGER.log(Level.SEVERE, "File: " + file.getName() + "\ndosent have momentum values (m0 and m2) at positions 3, 4 for all particles");
            this.particles = null;
            return false;
          }
          this.particles[counter].m0 = Float.parseFloat(particleInfo[2]);
          this.particles[counter].m2 = Float.parseFloat(particleInfo[3]);
        }
        counter++;
      }
      if (particlesInfo != null) particlesInfo.clear();
      return true;
    }

    /**
     * Creates a <code>ByteProcessor</code> and draws on it the particles defined in this MyFrame
     * <br>The background color is <code>Color.black</code>
     * <br>The color of the dots drawn for each particle is <code>Color.white</code>
     * <br>particles position have floating point precision but can be drawn only at integer precision -
     * therefore the created image is only an estimation
     *
     * @param width  defines the width of the created <code>ByteProcessor</code>
     * @param height defines the height of the created <code>ByteProcessor</code>
     * @return the created processor
     * @see ImageProcessor#drawDot(int, int)
     */
    private ImageProcessor createImage(int width, int height) {
      ImageProcessor ip = new ByteProcessor(width, height);
      ip.setColor(Color.black);
      ip.fill();
      ip.setColor(Color.white);
      for (int i = 0; i < this.particles.length; i++) {
        ip.drawDot(Math.round(this.particles[i].y), Math.round(this.particles[i].x));
      }
      return ip;
    }


  }

  class TrajectoryOriginal {


    Particle[] existingParticles;    // holds all particles of this trajetory in order
    int length;             // number of frames this trajectory spans on

    ArrayList gaps = new ArrayList();   // holds arrays (int[]) of size 2 that holds
    // 2 indexs of particles in the existingParticles.
    // These particles are the start and end points of a gap
    // in this trajectory
    int numGaps = 0;

    int serial_number;          // serial number of this trajectory (for report and display)
    Color color;            // the display color of this Trajectory
    Roi mouseSelectionArea;      // The Roi area where a mouse click will select this trajectory
    Roi focus_area;            // The Roi for focus display of this trajectory


    /**
     * Constructor.
     * <br>Constructs a Trajectory from the given <code>Particle</code> array.
     * <br>Sets its length according to information of the first and last particles
     * <br>Sets its <code>Color</code> to default (red)
     *
     * @param particles the array containing all the particles defining this Trajectory
     */
    public TrajectoryOriginal(Particle[] particles) {

      this.existingParticles = particles;
      // the length is the last trajectory frame - the first frame (first frame can be 0)
      this.length = this.existingParticles[this.existingParticles.length - 1].frame - this.existingParticles[0].frame;
      color = Color.red; //default
    }

    /**
     * Set the <code>focus_area</code> for this trajectory - it defines the area (ROI) focused
     * on when the user selects this trajectory to focus on
     * <br>The <code>focus_area</code> is an rectangular ROI that engulfs this trajectory
     * with 8 pixels margin from each edge
     * <p/>
     * //     * @see TrajectoryStackWindow#mousePressed(java.awt.event.MouseEvent)
     */
    void setFocusArea() {

  			/* find the min and max values of the x and y positions */
      float minX = this.existingParticles[0].x;
      float minY = this.existingParticles[0].y;
      float maxX = this.existingParticles[0].x;
      float maxY = this.existingParticles[0].y;
      for (int i = 0; i < this.existingParticles.length; i++) {
        minX = Math.min(this.existingParticles[i].x, minX);
        minY = Math.min(this.existingParticles[i].y, minY);
        maxX = Math.max(this.existingParticles[i].x, maxX);
        maxY = Math.max(this.existingParticles[i].y, maxY);
      }

  			/* set the focus area x, y , height, width to give focus area bigger by 8 pixels
         * then minimal rectangle surroundings this trajectory */

      // X and Y coordinates are not in the usual graph coordinates sense but in the image sense;
      // (0,0) is the upper left corner; x is vertical top to bottom, y is horizontal left to right
      int focusX = Math.max((int) minY - 8, 0);
      int focusY = Math.max((int) minX - 8, 0);
      int focusHeight = (int) maxX - focusY + 8;
      int focusWidth = (int) maxY - focusX + 8;
      // make sure that the -8 or +8 didnt create an ROI with bounds outside of the window
      if (focusX + focusWidth > originalImp.getWidth()) {
        focusWidth = originalImp.getWidth() - focusX;
      }
      if (focusY + focusHeight > originalImp.getHeight()) {
        focusHeight = originalImp.getHeight() - focusY;
      }
      this.focus_area = new Roi(focusX, focusY, focusWidth, focusHeight);
    }

    /**
     * Set the <code>mouseSelectionArea</code> for this trajectory - it defines the area (ROI)
     * on which a mouse click will add this trajectory as a candidate for selection
     * <br>When this trajectory is selected with a mouse click this ROI is highlighted for the user
     * to see his selection.
     * <br>The <code>mouseSelectionArea</code> is an rectangular ROI that engulfs this trajectory
     * with 1 pixel margin from each edge
     * <p/>
     * //     * @see TrajectoryStackWindow#mousePressed(java.awt.event.MouseEvent)
     */
    void setMouseSelectionArea() {

  			/* find the min and max values of the x and y positions */
      float minX = this.existingParticles[0].x;
      float minY = this.existingParticles[0].y;
      float maxX = this.existingParticles[0].x;
      float maxY = this.existingParticles[0].y;
      for (int i = 0; i < this.existingParticles.length; i++) {
        minX = Math.min(this.existingParticles[i].x, minX);
        minY = Math.min(this.existingParticles[i].y, minY);
        maxX = Math.max(this.existingParticles[i].x, maxX);
        maxY = Math.max(this.existingParticles[i].y, maxY);
      }

  			/* set the focus area x, y , height, width to give focus area bigger by 1 pixel
         * then minimal rectangle surroundings this trajectory */

      // X and Y coordinates are not in the usual graph coordinates sense but in the image sense;
      // (0,0) is the upper left corner; x is vertical top to bottom, y is horizontal left to right
      int focusX = (int) minY - 1;
      int focusY = (int) minX - 1;
      int focusHeight = (int) maxX - focusY + 1;
      int focusWidth = (int) maxY - focusX + 1;
      this.mouseSelectionArea = new Roi(focusX, focusY, focusWidth, focusHeight);

    }

    /**
     * Populates the <code>gaps</code> Vector with int arrays of size 2.
     * <br>Each array represents a gap, while the values in the array are the <b>indexs</b>
     * of the particles that have a gap between them.
     * <br>The index is of the particles in the <code>existingParticles</code> array -
     * two sequential particles that are more then 1 frame apart give a GAP
     */
    void populateGaps() {

      for (int i = 0; i < existingParticles.length - 1; i++) {
        // if two sequential particles are more then 1 frame apart - GAP
        if (existingParticles[i + 1].frame - existingParticles[i].frame > 1) {
          int[] gap = {i, i + 1};
          gaps.add(gap);
          numGaps++;
        }
      }
    }


    private void animate(int magnification, int removed_frames) {

      int current_frame;
      int previous_frame = existingParticles[0].frame - removed_frames;
      for (int i = 0; i < existingParticles.length; i++) {
        current_frame = existingParticles[i].frame + 1 - removed_frames;
        while (current_frame - previous_frame > 1) {
          previous_frame++;
          draw4Dynamic(trajectoryStack.getProcessor(previous_frame), i, magnification);
          drawGaps4Dynamic(trajectoryStack.getProcessor(previous_frame), i, magnification);
        }
        // if some frames were removed from trajectoryStack then the frame number
        // of a particle will not correspond with frame number in the stack
        // by subtracting the number of removed frames from the particle frame number
        // we will get the right frame in trajectoryStack
        draw4Dynamic(trajectoryStack.getProcessor(current_frame), i, magnification);
        drawGaps4Dynamic(trajectoryStack.getProcessor(current_frame), i, magnification);
        previous_frame = current_frame;
      }
    }


    private void draw4Dynamic(ImageProcessor ip, int last_frame, int magnification) {

      ip.setColor(this.color);
      if (last_frame >= existingParticles.length) {
        //				 TODO error
      }
      if (existingParticles.length < 2) {
        //				 TODO error
      }
      ip.setLineWidth(1);
      int i = Math.max(0, last_frame - trajectoryTail);

      ip.moveTo(getXDisplayPosition(this.existingParticles[i].y, magnification),
          getYDisplayPosition(this.existingParticles[i].x, magnification));
      i++;
      ip.lineTo(getXDisplayPosition(this.existingParticles[i].y, magnification),
          getYDisplayPosition(this.existingParticles[i].x, magnification));
      for (i++; i <= last_frame; i++) {
        ip.drawLine(getXDisplayPosition(this.existingParticles[i - 1].y, magnification),
            getYDisplayPosition(this.existingParticles[i - 1].x, magnification),
            getXDisplayPosition(this.existingParticles[i].y, magnification),
            getYDisplayPosition(this.existingParticles[i].x, magnification));
      }
    }

    /**
     * Converts a floating-point offscreen x-coordinate (particle position) to a <code>trajectoryStack</code>
     * actual screen x-coordinate as accurate as possible according to the magnification of the
     * display while taking into account that the <code>trajectoryStack</code> display can be only a part
     * of the original image
     * <br> since ImageJ doesn't work with floating point precision - rounding is also applied
     *
     * @param particle_position floating-point offscreen x-coordinate (particle position <b>Y</b>)
     * @param magnification     the magnification factor for the <code>trajectoryStack</code>
     * @return the converted coordinate
     */
    private int getXDisplayPosition(float particle_position, int magnification) {

      int roi_x = 0;
      if (trajectoryStack.getHeight() != originalImp.getStack().getHeight() ||
          trajectoryStack.getWidth() != originalImp.getStack().getWidth()) {
        roi_x = IJ.getImage().getRoi().getBounds().x;
      }
      particle_position = (particle_position - roi_x) * magnification + (float) (magnification / 2.0) - (float) 0.5;
      return Math.round(particle_position);
    }

    /**
     * Converts a floating-point offscreen y-coordinate (particle position) to a <code>trajectoryStack</code>
     * actual screen y-coordinate as accurate as possible according to the magnification of the
     * display while taking into account that the <code>trajectoryStack</code> display can be only a part
     * of the original image
     * <br> since ImageJ doesn't work with floating point precision - rounding is also applied
     *
     * @param particle_position floating-point offscreen y-coordinate (particle position <b>X</b>)
     * @param magnification     the magnification factor for the <code>trajectoryStack</code>
     * @return the converted coordinate
     */
    private int getYDisplayPosition(float particle_position, int magnification) {

      int roi_y = 0;
      if (trajectoryStack.getHeight() != originalImp.getStack().getHeight() ||
          trajectoryStack.getWidth() != originalImp.getStack().getWidth()) {
        roi_y = IJ.getImage().getRoi().getBounds().y;
      }
      particle_position = (particle_position - roi_y) * magnification + (float) (magnification / 2.0) - (float) 0.5;
      return Math.round(particle_position);
    }

    /**
     * Draws a red line for all <code>gaps</code> in this <code>trajectory</code> in the range
     * of <code>trajectoryTail</code> from the <code>Particle</code> in the given location
     * (particles[particle_index])
     * <br>Draws directly (modifys) on the given <code>ip</code> (ImageProcessor)
     * <br>This method is for generating a <b>progressive</b> trajectory view
     *
     * @param ip             ImageProcessor to draw the gap on
     * @param particle_index index of the last particle until which gaps will be drawn
     * @param magnification  the magnification factor of the image to draw on
     * @see #getXDisplayPosition(float, int)
     * @see #getYDisplayPosition(float, int)
     */
    private void drawGaps4Dynamic(ImageProcessor ip, int particle_index, int magnification) {

      if (gaps == null) return;
        /* set ip color to gaps color (RED) */
      ip.setColor(Color.red);
      Object[] gaps_tmp = gaps.toArray();

  			/* go over all gaps in this trajectory*/
      for (int i = 0; i < numGaps; i++) {

        // gaps_tmp is now an array of int[] (of size 2)
        // each int[] holds 2 indexs of particles in the existingParticles.
        int start_particle_index = ((int[]) gaps_tmp[i])[0];
        int end_particle_index = ((int[]) gaps_tmp[i])[1];

        // only if this gap is in the range of the (particle at the given index - trajectoryTail)
        if (start_particle_index < particle_index && start_particle_index > particle_index - trajectoryTail) {
          ip.drawLine(getXDisplayPosition((this.existingParticles[start_particle_index]).y, magnification),
              getYDisplayPosition((this.existingParticles[start_particle_index]).x, magnification),
              getXDisplayPosition((this.existingParticles[end_particle_index]).y, magnification),
              getYDisplayPosition((this.existingParticles[end_particle_index]).x, magnification));
        }
      }
        /* set ip color back to this trajectory color */
      ip.setColor(this.color);
    }

    /**
     * Generates a "ready to print" string with the particles defined
     * in this trajectory in the right order.
     *
     * @return a String with the info
     */
    public String toString() {
      return toStringBuffer().toString();
    }

    /**
     * The method <code>toString()</code> calls this method
     * <br>Generates a "ready to print" StringBuffer with the particles defined
     * in this trajectory in the right order
     *
     * @return a <code>StringBuffer</code> with the info
     * @see Particle#toStringBuffer()
     */
    public StringBuffer toStringBuffer() {
      StringBuffer s = new StringBuffer();
      for (int i = 0; i < existingParticles.length; i++) {
        s.append(existingParticles[i].toStringBuffer());
      }
      s.append("\n");
      return s;
    }

    public java.util.List<String> toCSV(final int id) {
      java.util.List<String> lines = new ArrayList<String>(existingParticles.length);

      for (Particle particle : existingParticles) {
        String line = String.format("%s,%s,%s,%s", id, particle.frame, particle.x, particle.y);
        lines.add(line);
      }

      return lines;
    }

  }

}
