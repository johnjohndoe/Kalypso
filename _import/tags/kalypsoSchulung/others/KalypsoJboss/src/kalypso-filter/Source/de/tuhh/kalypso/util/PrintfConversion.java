// PrintfConversion.java
//
//   A container defining a prinf conversion specification.
//
//   senger@ebi.ac.uk
//   March 1998
//

package de.tuhh.kalypso.util;

/** A container defining a <em>prinf-conversion</em> specification.
 * @see Printf
 * @author <A HREF="mailto:senger@ebi.ac.uk">Martin Senger</A>
 * @version $Id$
 */
public final class PrintfConversion {

    /** type of conversion specification */
    public char type;

    /** minimum field width */
    public int  minWidth;

    /** precision.
     * <P>
     * for string - maximum number of bytes,
     * for the others - minimum number of digits
     */
    public int  precision;

    /** alignment */
    public int  align;

    /** padding character */
    public char pad;

    /** offset in the formatting string where this conversion starts from */
    public int  offset;

    /** length of this conversion in the formatting string */
    public int  length;

    /**************************************************************************
     *
     * Constants
     *
     **************************************************************************/

    // types of conversions
    /** string conversion */    public static final char STRING  = 's';
    /** integer conversion */   public static final char INT     = 'd';
    /** float conversion */     public static final char FLOAT   = 'f';
    /** character conversion */ public static final char CHAR    = 'c';
    /** percent character */    public static final char PERCENT = '%';
    /** error conversion */     public static final char ERROR   = ' ';
    /** no conversion */        public static final char NONE    = '-';

    /** type of alignment - left alignment */
    public static final int LEFT  = 0;
    /** type of alignment - right alignment */
    public static final int RIGHT = 1;

    /**************************************************************************
     * No-arg constructor
     **************************************************************************/
    public PrintfConversion () {
    }

    /**************************************************************************
     *
     **************************************************************************/
    public PrintfConversion (char type,
                             int  minWidth,
                             int  precision,
                             int  align,
                             char pad,
                             int  offset,
                             int  length
			     ) {
	this.type = type;
	this.minWidth = minWidth;
	this.precision = precision;
	this.align = align;
	this.pad = pad;
	this.offset = offset;
	this.length = length;
    }

    /**************************************************************************
     *
     **************************************************************************/
    public PrintfConversion (int  offset,
			     int  length
			     ) {
	this.type = NONE;
	this.minWidth = 0;
	this.precision = 0;
	this.align = RIGHT;
	this.pad = ' ';

	this.offset = offset;
	this.length = length;
    }

}
