package core;

import util.LogFormatter;

import java.io.*;
import java.util.ArrayList;
import java.util.Properties;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Main entry point into the program.
 * <p>
 * Generates the report based an a directory containing all the individual frames and generates a report of the
 * trajectories.
 * </p>
 * User: vader
 * Date: 12/04/2013
 */
public class ParticleReader {

  private static Logger logger;
  private static boolean includeHeaders = false;

  public ParticleReader() {
    formatLogger();
  }

  /**
   * @param args mandatory two arguments 1. input directory of the image frames 2. full path and name of the output file
   * @throws Exception if particle.properties can't load or of the program encounters a runtime or unchecked exception
   */
  public static void main(final String... args) throws Exception {

    ParticleReader reader = new ParticleReader();

    if (args == null || args.length < 2) {
      throw new ExceptionInInitializerError("Program requires both the input and output file name");
    }


    String inputFileName = args[0];
    String outputFileName = args[1];

    reader.generateReport(inputFileName, outputFileName);

    System.out.printf("Done");
  }

   ArrayList<ParticleTrackerOriginal.TrajectoryOriginal> generateReport(final String inputFileName, final String outputFileName) throws IOException {
    logger.info(String.format("Running report with input=%s, output=%s", inputFileName, outputFileName));

    loadDefaultProperties();


    logger.log(Level.INFO, String.format("Reading input file %s", inputFileName));

    File inputFile = new File(inputFileName);
    if (!inputFile.exists() || !inputFile.canRead()) {
      throw new FileNotFoundException(String.format("File %s can not be found or is not readable", inputFileName));
    }

      ParticleTrackerOriginal original = new ParticleTrackerOriginal(inputFile);
    if (inputFile.isDirectory()) {
      original.run(new File(outputFileName), includeHeaders);
    } else {
      logger.warning(String.format("Expect location=%s to be a directory", inputFile.getAbsoluteFile()));
      System.exit(0);
    }
   return original.getAllTraj();
   }

  /**
   * Java logger writes log lines to two lines per log, create a custom logger to improve the default readability
   */
  private static void formatLogger() {
    LogFormatter formatter = new LogFormatter();
    logger = Logger.getLogger("global");
    logger.setUseParentHandlers(false);
    ConsoleHandler handler = new ConsoleHandler();
    handler.setFormatter(formatter);
    logger.addHandler(handler);

  }


  /**
   * Loads the default properties from particle.properties<br />
   * If there are any overridden properties I.e -Dparticle.displacement=10.0 on the command line then these will take
   * presidence over the properties file.
   *
   * @throws IOException if the particle.properties can not be loaded.
   */
  void loadDefaultProperties() throws IOException {
    final String propertiesFile = "particle.properties";

    String defaultPropertiesPath = String.format("src%smain%sresources%s%s", File.separator, File.separator, File.separator, propertiesFile);
    File f = new File(defaultPropertiesPath);
    Properties properties = new Properties();
    if (existsAndCanRead(f)) {
      logger.info(String.format("Loading properties from file=%s", defaultPropertiesPath));
      properties.load(new FileInputStream(f));
    } else {
      InputStream resourceAsStream = ParticleReader.class.getResourceAsStream("/particle.properties");
      properties.load(resourceAsStream);
      logger.info(String.format("Loading properties file=%s", defaultPropertiesPath));
    }

    //Check if there are any custom properties set, of so override the defaults
    for (String key : properties.stringPropertyNames()) {
      setProperty(key, properties);
    }

    logger.info(">>Properties used<<");
    for (String key : properties.stringPropertyNames()) {
      logger.info(String.format("property=%s value=%s", key, properties.get(key)));
    }

    System.getProperties().putAll(properties);

    includeHeaders = System.getProperty("particle.report.showheaders") == null ? false : Boolean.valueOf(System.getProperty("particle.report.showheaders"));

  }

  private void setProperty(final String key, Properties properties) {


    String value = System.getProperty(key);
    if (value != null) {
      logger.info(String.format("Using user provided property key=%s value=%s", key, value));
      properties.setProperty(key, value);
    }


  }

  private boolean existsAndCanRead(final File f) {
    return f != null && f.exists() && f.canRead();
  }

}
