package core;

import java.text.DecimalFormat;



/**
 * Defines a particle that holds all the relevant info for it.
 * A particle is detected in an image or given as input in test file mode
 * 		X and Y coordinates are not in the usual graph coordinates sense but in the image sense;
 * 		(0,0) is the upper left corner
 *  	x is vertical top to bottom
 *  	y is horizontal left to right
 */
public class Particle {

	public float x, y, z; 					// the originally given coordinates - to be refined
	public float original_x; 				// the originally given coordinates - not to be changed
	public float original_y, original_z;
	int frame; 						// the number of the frame this particle belonges to (can be 0)
	public boolean special; 				// a flag that is used while detecting and linking particles
	public int[] next; 						// array that holds in position i the particle number in frame i
											// that this particle is linked to
	public int nbIterations = 0; //debug
	/* only relevant to particles detected in images */
	public float m0;						// intensity moment
	public float m1, m2, m3, m4;
	public float score; 					// non-particle discrimination score


	/* only relevant to particles given as input */
	public String[] all_params; 			// all params that relate to this particle,
											// 1st 2 should be x and y respectfully
	int linkrange;
	/**
	 * constructor.
	 * @param x - original x coordinates
	 * @param y - original y coordinates
	 * @param frame_num - the number of the frame this particle belonges to
	 */
	public Particle (float x, float y, float z, int frame_num, int linkrange) {
		this.x = x;
		this.original_x = x;
		this.y = y;
		this.original_y = y;
		this.z = z;
		this.original_z = z;
		this.special = true;
		this.setFrame(frame_num);
		this.linkrange = linkrange;
		this.next = new int[linkrange];
	}

  /**
		 * constructor for particles created from text files.
		 * @param x - original x coordinates
		 * @param y - original y coordinates
		 * @param frame_num - the number of the frame this particle is in
		 * @param params - all params that relate to this particle, first 2 should be x and y respectfully
		 */
		public Particle (float x, float y, int frame_num, String[] params) {
			this.x = x;
			this.original_x = x;
			this.y = y;
			this.original_y = y;
			this.all_params = params;
			this.special = true;
			this.frame = frame_num;
			this.next = new int[linkrange];
			this.score = 0.0F;
			this.m0 = 0.0F;
			this.m2 = 0.0F;
		}

	/**
	 * constructor for particles created from text files.
	 * @param x - original x coordinates
	 * @param y - original y coordinates
	 * @param frame_num - the number of the frame this particle is in
	 * @param params - all params that relate to this particle, first 2 should be x and y respectfully
	 */
	public Particle (float x, float y, float z, int frame_num, String[] params, int linkrange) {
		this.x = x;
		this.original_x = x;
		this.y = y;
		this.original_y = y;
		this.z = z;
		this.original_z = z;
		this.all_params = params;
		this.special = true;
		this.setFrame(frame_num);
		this.linkrange = linkrange;
		this.next = new int[linkrange];
		this.score = 0.0F;
		this.m0 = 0.0F;
		this.m1 = 0.0F;
		this.m2 = 0.0F;
		this.m3 = 0.0F;
		this.m4 = 0.0F;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return toStringBuffer().toString();
	}

	/**
	 * The method <code>toString()</code> calls this method
	 * <br>Generates (in real time) a "ready to print" <code>StringBuffer</code> with information
	 * about this Particle:
	 * <ul>
	 * <li> frame
	 * <li> x
	 * <li> y
	 * <li> m0
	 * <li> m2
	 * <li> score
	 * </ul>
	 * For text files mode - just prints all the information given for the particles
	 * @return a StringBuffer with this infomation
	 */
	public StringBuffer toStringBuffer() {

		// I work with StringBuffer since its faster than String
		// At the end convert to String and return
		StringBuffer sb = new StringBuffer();
		StringBuffer sp = new StringBuffer(" ");

		// format the number to look nice in print (same number of digits)
//		NumberFormat nf = NumberFormat.getInstance();
//		nf.setMaximumFractionDigits(6);
//		nf.setMinimumFractionDigits(6);
		DecimalFormat nf = new DecimalFormat("######0.000000");
		nf.setGroupingUsed(false);
		sb.append(this.getFrame());

		sb.append(sp);
		sb.append(nf.format(this.x));
		sb.append(sp);
		sb.append(nf.format(this.y));
		sb.append(sp);
		sb.append(nf.format(this.z));
		sb.append(sp);
		sb.append(nf.format(this.m0));
		sb.append(sp);
		sb.append(nf.format(this.m1));
		sb.append(sp);
		sb.append(nf.format(this.m2));
		sb.append(sp);
		sb.append(nf.format(this.m3));
		sb.append(sp);
		sb.append(nf.format(this.m4));
		sb.append(sp);
		sb.append(nf.format(this.score));
		sb.append("\n");

		return sb;
	}

	public void setFrame(int frame) {
		this.frame = frame;
	}

	public int getFrame() {
		return frame;
	}

	public double[] getPosition() {
		double[] result =  {(double) x,(double) y,(double) z};
		return result;
	}
}

