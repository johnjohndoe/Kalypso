package de.tuhh.kalypso.util;

import java.util.*;
import java.text.*;

// Printf.java
//
//   Methods dealing with printf format specification.
//
//   senger@ebi.ac.uk
//   March 1998
//
//   $Id$
//



/** An abstract class dealing with printf-like specification.
 * It allows formatting strings according to the rules similar to the
 * C function <em>printf</em>.
 *<P>
 * It also gives access to the structure of the formatting strings
 * and thus allows to se formats for other purposes than only
 * for pure formatting. An example may be a graphical user interface
 * displaying fixed parts of the format string as labels or read-only
 * data, while allowing to enter variable parts - but validating
 * their format.
 *<P>
 * The most important part is a <b>format</b> - a string specifying
 * how one or more values should be formatted. The <em>format</em>
 * string consists from <b>slices</b>. <em>Slices</em> are of two
 * kinds: <b>printf-conversions</b> and the texts in between
 * <em>conversions</em>. For example, the format:
 * <PRE>
 *    %-8.5d Mary %s Poppins
 * </PRE>
 * consists from four <em>slices</em>:
 * <PRE>
 *    %-8.5d
 *    Mary
 *    %s
 *    Poppins
 * </PRE>
 *<P>
 * Each <em>printf-conversion</em> has one of the following formats:
 * <PRE>
 *    %[-][<em>digits1</em>][<em>.digits2</em>]<em>type</em>
 *    %[0][<em>digits1</em>][<em>.digits2</em>]<em>type</em>
 * </PRE>
 * where an optional "-" means left alignments (by default the resulting value
 * is aligned to the right), a zero at the beginning means that a zero
 * character (0) is used as the padding character, and <em>type</em> and
 * <em>digits</em> fields mean:
 * <blockquote>
 * <table border=3 cellpadding=3 cellspacing=3>
 * <tr>
 *  <th>type</th><th>meaning</th><th>digits1</th><th>digits2</th>
 * </tr>
 * <tr>
 *  <td valign=top><b>s</b> <td valign=top><b>String value</b></td>
 *  <td> minimal length (padded always by spaces, never by zeroes)</td>
 *  <td> maximum length (exceeding characters are cut off and the string is shortened)</td>
 * </tr>
 * <tr>
 *  <td valign=top><b>c</b> <td valign=top><b>Character value</b></td>
 *  <td> minimal length (padded always by spaces, neber by zeros</td>
 *  <td> <em>ignored</em></td>
 * </tr>
 * <tr>
 *  <td valign=top><b>d</b> <td valign=top><b>Integer value</b>
 *       (if the original value is fractional it is rounded to the nearest integer first)</td>
 *  <td> minimal length (padded to the left if necessary</td>
 *  <td> minimum number of digits to appear in the results</td>
 * </tr>
 * <tr>
 *  <td valign=top><b>f</b> <td valign=top><b>Floating-point value</b></td>
 *  <td> minimal length (padded to left by zeros if necessary but never truncated on the left)</td>
 *  <td> minimal number of fractional digits to appear in the result (attention: default is 3!)</td>
 * </tr>
 * </table>
 * </blockquote>
 *
 * Here are some examples of printf formatting (note that quoting characters
 * shown in the result column are not part of the result values, and "_"
 * underscore characters are there instead of spaces):
 * <blockquote>
 * <table border=1>
 * <tr> <td><b>format</b></td> <td><b>value</b></td> <td><b>result</b></td> <td><b>note</b></td> </tr>
 * <tr> <td>%d</td>      <td>123</td>      <td>"123"</td>        <td>&nbsp;</td> </tr>
 * <tr> <td>%.5d</td>    <td>123</td>      <td>"00123"</td>      <td>&nbsp;</td> </tr>
 * <tr> <td>%8.5d</td>   <td>123</td>      <td>"___00123"</td>   <td>&nbsp;</td> </tr>
 * <tr> <td>%08.5d</td>  <td>123</td>      <td>"00000123"</td>   <td>&nbsp;</td> </tr>
 * <tr> <td>%-8.5d</td>  <td>123</td>      <td>"00123___"</td>   <td>&nbsp;</td> </tr>
 * <tr> <td>%8.5d</td>   <td>123.56</td>   <td>"___00124"</td>   <td>&nbsp;</td> </tr>
 * <tr> <td>&nbsp;</td>  <td>&nbsp;</td>   <td>&nbsp;</td>       <td>&nbsp;</td> </tr>
 * <tr> <td>%f</td>      <td>123.4567</td> <td>"123.457"</td>    <td>because default precision is 3</td> </tr>
 * <tr> <td>%10.5f</td>  <td>123.4567</td> <td>"_123.45670"</td> <td>&nbsp;</td> </tr>
 * <tr> <td>%010.5f</td> <td>123.4567</td> <td>"0123.45670"</td> <td>&nbsp;</td> </tr>
 * <tr> <td>%-10.5f</td> <td>123.4567</td> <td>"123.45670_"</td> <td>&nbsp;</td> </tr>
 * <tr> <td>%1f</td>     <td>123.4567</td> <td>"123.456"</td>    <td>because it never truncates on the left</td> </tr>
 * <tr> <td>&nbsp;</td>  <td>&nbsp;</td>   <td>&nbsp;</td>       <td>&nbsp;</td> </tr>
 * <tr> <td>%s</td>      <td>AppLab</td>   <td>"AppLab"</td>     <td>&nbsp;</td> </tr>
 * <tr> <td>%10s</td>    <td>AppLab</td>   <td>"____AppLab"</td> <td>&nbsp;</td> </tr>
 * <tr> <td>%-10s</td>   <td>AppLab</td>   <td>"AppLab____"</td> <td>&nbsp;</td> </tr>
 * <tr> <td>%.4s</td>    <td>AppLab</td>   <td>"AppL"</td>       <td>&nbsp;</td> </tr>
 * <tr> <td>%10.4s</td>  <td>AppLab</td>   <td>"______AppL"</td> <td>&nbsp;</td> </tr>
 * <tr> <td>%-10.4s</td> <td>AppLab</td>   <td>"AppL______"</td> <td>&nbsp;</td> </tr>
 * <tr> <td>&nbsp</td>   <td>&nbsp</td>    <td>&nbsp;</td>       <td>&nbsp;</td> </tr>
 * <tr> <td>%c</td>      <td>AppLab</td>   <td>"A"</td>          <td>&nbsp;</td> </tr>
 * </table>
 * </blockquote>
 *
 * @see PrintfConversion
 * @see TestPrintf
 * @author <A HREF="mailto:senger@ebi.ac.uk">Martin Senger</A>
 * @version $Id$
 */
public abstract class Printf {
	
    /** Version and date of the last update. */
    public static final String VERSION = "$Id$";
	
    static NumberFormat nf;       // this will make the conversion
    static int          minInt;   // here store the original values
    static int          minFrac;
    static int          maxFrac;
	
    /**************************************************************************
	 * static initializer
	 **************************************************************************/
    static {
		nf = NumberFormat.getInstance();
		minInt = nf.getMinimumIntegerDigits();
		minFrac = nf.getMinimumFractionDigits();
		maxFrac = nf.getMaximumFractionDigits();
    }
	/** This method allows the user to set the number format from within an other
	 * class. Solves the problem of english(.)/german(,) dezimal.
	 * @param l The number format ex: Locale.GERMAN or Locale.ENGLISH.
	 */
	public static void setLocale( Locale l )
	{
		nf = NumberFormat.getInstance( l );
	}
	
	
    private static char[] availableConversions = { PrintfConversion.CHAR,
			PrintfConversion.INT,
			PrintfConversion.FLOAT,
			PrintfConversion.STRING };
	
    /**************************************************************************
	 * Parse the formatting string and return the first printf-conversion
	 * found there.
	 * <P>
	 * @param format formatting string
	 * @return found conversion or null if there is no conversion specification.
	 **************************************************************************/
    public static PrintfConversion getConversion (String format) {
		return getConversion (format, 0);
    }
	
    /**************************************************************************
	 * Parse the formatting string from the given position and return the first
	 * printf-conversion found there.
	 * <P>
	 * Recognised and supported format of a conversion specification
	 * (no whitespaces used, they are here only for readability) are:
	 * <PRE>
	 *     % [-|0] [digits] [.] [digits] c|d|f|s|%
	 * </PRE>
	 *
	 * @param format formatting string
	 * @param offset starting possition
	 * @return found conversion or null if there is no conversion specification.
	 **************************************************************************/
    public static PrintfConversion getConversion (String format, int offset) {
		
		char c;
		String types = new String (availableConversions);
		PrintfConversion p = new PrintfConversion (PrintfConversion.ERROR, 0, 0, PrintfConversion.RIGHT, ' ', -1, 0);
		
		int pos = format.indexOf ("%", offset);
		if (pos == -1) return null;    // no conversion spec found
		p.offset = pos;
		
		int formatLength = format.length() - 1;
		if (pos == formatLength) return null;  // a % found at the end of the string
		pos++;
		
		// first character can be a modifier (or a %-character itself)
		c = format.charAt (pos);
		if (c == '0') {
			p.pad = '0';
			pos++;
		} else if (c == '-') {
			p.align = PrintfConversion.LEFT;
			pos++;
		} else if (c == '%') {
			p.type = PrintfConversion.PERCENT;
			p.length = 2;
			return p;
		}
		
		// then iterate until a conversion type is found
		String digits = "0123456789";
		StringBuffer num = new StringBuffer();  // here we will collect a series of digits
		boolean precision = false;   // here we will remember that a 'dot' was found
		while (pos <= formatLength) {
			c = format.charAt (pos);
			if (types.indexOf (c) > -1) {
				p.type = c;
				if (precision)
					p.precision = toInt (num);
				else
					p.minWidth = toInt (num);
				p.length = pos + 1 - p.offset;
				return p;
			}
			if (c == '.') {
				p.minWidth = toInt (num);
				precision = true;
				num = new StringBuffer();
			} else if (digits.indexOf (c) > -1) {
				num.append (c);
			} else {
				p.type = PrintfConversion.ERROR;
				p.length = pos + 1 - p.offset;
				return p;
			}
			pos++;
		}
		
		return null;   // what are you doing here?
	}
	
    /**************************************************************************
	 * Parse the formatting string and return all printf-conversions found there.
	 * <P>
	 * @see #getConversion(String) getConversion (format)
	 * @see #getConversion(String,int) getConversion (format, offset)
	 * @param format formatting string
	 * @return found conversions or an empty array if there were no conversion
	 *         specifications.
	 **************************************************************************/
    public static PrintfConversion[] getConversions (String format) {
		Vector v = new Vector();
		int pos = 0;
		while (true) {
			PrintfConversion p = getConversion (format, pos);
			if (p == null) break;
			v.addElement (p);
			pos = p.offset + p.length;
		}
		PrintfConversion[] result = new PrintfConversion [v.size()];
		v.copyInto (result);
		return result;
    }
	
    /**************************************************************************
	 * getSlices()
	 * Parse the formatting string and return all printf-conversions found there,
	 * and all pieces between them. The pieces between conversions are
	 * labeled with conversion type {@link PrintfConversion#NONE NONE}.
	 *<P>
	 * @see #getConversions
	 * @param format formatting string
	 * @return all slices of the given format
	 **************************************************************************/
    public static PrintfConversion[] getSlices (String format) {
		Vector v = new Vector();
		int formatLen = format.length();
		PrintfConversion pNone;
		int pos = 0;
		while (pos < formatLen) {
			PrintfConversion p = getConversion (format, pos);
			if (p == null) {
				v.addElement (new PrintfConversion (pos, formatLen - pos));
				break;
			} else {
				if (p.offset > pos)
					v.addElement (new PrintfConversion (pos, p.offset - pos));
				v.addElement (p);
			}
			pos = p.offset + p.length;
		}
		PrintfConversion[] result = new PrintfConversion [v.size()];
		v.copyInto (result);
		return result;
    }
	
    /**************************************************************************
	 * Format given list of values by given format.
	 * All printf-conversions from the <tt>format</tt> are substituted by
	 * <tt>values</tt> (each conversion by one value) by applying
	 * appropriate conversion rules. If there is more values than printf-
	 * conversions, or more conversions than values, the rest is ignored.
	 * <P>
	 * @see #convert
	 * @param format a formatting string
	 * @param values to be formatted
	 * @return a formatted string with substitued conversions
	 **************************************************************************/
    public static String format (String format, String[] values) {
		StringBuffer s = new StringBuffer();   // here we build a result
		int lastPos = 0;   // remember where we are in the 'format'
		PrintfConversion[] p = getConversions (format);
		int j = 0;    // current index into 'values'
		for (int i = 0; i < p.length; i++) {
			s.append (format.substring (lastPos, p[i].offset));
			lastPos = p[i].offset + p[i].length;
			if (p[i].type == PrintfConversion.PERCENT) {
				s.append ('%');
				continue;
			}
			if (p[i].type == PrintfConversion.ERROR) {
				s.append (format.substring (p[i].offset, p[i].offset + p[i].length));
				continue;
			}
			if (values.length > j) s.append (convert (p[i], values[j]));
			j++;
		}
		s.append (format.substring (lastPos));
		return new String (s);
    }
	
    /**************************************************************************
	 * Format given value by a given format.
	 * <P>
	 * @see #convert
	 * @see #format(String,String[]) format (format, values)
	 * @param format a formatting string
	 * @param value to be formatted
	 * @return a formatted string
	 **************************************************************************/
    public static String format (String format, String value) {
		return format (format, new String[] { value });
    }
	
    /**************************************************************************
	 * Scan a formatted value and try to identify individual values from
	 * which the string was created, according the given format.
	 * It is the opposite what <tt>format</tt> and <tt>convert</tt> do.
	 * <P>
	 * It does a similar thing as the C function <tt>scan</tt> but it is much
	 * more limited: the given value must strictly correspond with the format
	 * (additional whitespaces may bring unexpected results).
	 * <P>
	 * @see #format(String,String[]) format
	 * @see #convert
	 * @param format formatting string
	 * @param value formatted value
	 * @return return an array of extracted individual values (as strings)
	 **************************************************************************/
    public static String[] scan (String format, String value) {
		Vector v = new Vector();
		
		int pos = 0, nextPos = 0, len;
		PrintfConversion[] s = getSlices (format);
		int valueLen = value.length();
		for (int i = 0; i < s.length; i++) {
			
			// here we have a real conversion, not just a filler text
			if (s[i].type != PrintfConversion.NONE && s[i].type != PrintfConversion.ERROR) {
				
				// if this is the last slice, take the rest of the scanned value
				// without considering the conversion specification (which
				// need not to be of any good anyway, for example, a simple %d
				// would not tell you the length of the corresponding value)
				if (i+1 == s.length) {
					nextPos = valueLen;
					v.addElement (value.substring (pos, nextPos));
					
					// if next slice is a filler text, try to search for it directly
					// (doing it this way we can find values which are longer then their
					// specified minimal length in conversion definition)
				} else if (s[i+1].type == PrintfConversion.NONE || s[i+1].type == PrintfConversion.ERROR) {
					nextPos = value.indexOf (format.substring (s[i+1].offset,
															   s[i+1].offset + s[i+1].length),
											 pos);
					if (nextPos == -1) break;   // error
					v.addElement (value.substring (pos, nextPos));
					
					// if the length of this slice is not specified in the conversion,
					// we can still try to "eat" only as many characters from the input value,
					// as they correspond with the current conversion type
				} else if (s[i].minWidth == 0 && s[i].precision == 0) {
					if (s[i].type == PrintfConversion.INT || s[i].type == PrintfConversion.FLOAT) {
						NumberFormat nuf = NumberFormat.getInstance();
						nf.setParseIntegerOnly (s[i].type == PrintfConversion.INT);
						ParsePosition pp = new ParsePosition (pos);
						Number number = nuf.parse (value, pp);
						nextPos = pp.getIndex();
						if (nextPos > pos)
							v.addElement (number.toString());    // a number found, use it
						else
							v.addElement (value.substring (pos, nextPos));  // failed to find any number
					} else {
						char c;
						while (nextPos < valueLen) {
							c = value.charAt (nextPos);
							if (Character.isDigit (c)) break;
							nextPos++;
						}
						v.addElement (value.substring (pos, nextPos));
					}
					
					// otherwise we have to rely on lenght specified in conversion
					// (we do it by formatting a "0" a see how long it is)
				} else {
					len = (format (format.substring (s[i].offset, s[i].offset + s[i].length), "0")).length();
					nextPos = Math.min (pos + len, valueLen);
					v.addElement (value.substring (pos, nextPos));
				}
				
				// and here we just skip a filler text
			} else {
				nextPos = pos + s[i].length;
			}
			
			pos = nextPos;     // jump to a next slices on 'value'
			if (nextPos >= valueLen) break;
			
		}
		
		String[] values = new String [v.size()];
		v.copyInto (values);
		return values;
    }
	
    /**************************************************************************
	 * Convert one <tt>value</tt> to a formatted string according to a given
	 * conversion specification.
	 *<P>
	 * @see #format
	 * @param p a formatting conversion
	 * @param value a formatted value
	 * @return formatted string
	 **************************************************************************/
    public static String convert (PrintfConversion p, String value) {
		switch (p.type) {
			case PrintfConversion.INT:
				nf.setMinimumIntegerDigits  (p.precision == 0 ? minInt : p.precision);
				nf.setMinimumFractionDigits (0);
				nf.setMaximumFractionDigits (0);
				nf.setGroupingUsed (false);
				return pad (nf.format (toDouble (value)), p.align, p.minWidth, p.pad);
			case PrintfConversion.FLOAT:
				nf.setMinimumIntegerDigits  (minInt);
				nf.setMinimumFractionDigits (p.precision == 0 ? minFrac : p.precision);
				nf.setMaximumFractionDigits (p.precision == 0 ? maxFrac : p.precision);
				nf.setGroupingUsed (false);
				return pad (nf.format (toDouble (value)), p.align, p.minWidth, p.pad);
			case PrintfConversion.CHAR:
				p.precision = 1;   // and continue to STRING branch
			case PrintfConversion.STRING:
				if (p.precision > 0 && value.length() >= p.precision)
					value = value.substring (0, p.precision);
				return pad (value, p.align, p.minWidth, ' ');
		}
		return "";
    }
	
    /**************************************************************************
	 * pad()
	 *   Return 'str' padded to the left/right (depending on 'align') by 'padChar'.
	 *   The desired length of the padded string is 'desiredLength'.
	 **************************************************************************/
    private static String pad (String str, int align, int desiredLength, char padChar) {
		int padLen = desiredLength - str.length();
		if (padLen > 0) {
			char[] padding = new char [ padLen ];
			for (int i = 0; i < padding.length; i++)
				padding[i] = padChar;
			if (align == PrintfConversion.LEFT)  return str + new String (padding);
			if (align == PrintfConversion.RIGHT) return new String (padding) + str;
		}
		return str;
    }
	
    /**************************************************************************
	 * toInt()
	 *   Return int created from a string.
	 **************************************************************************/
    private static int toInt (StringBuffer str) {
		int i;
		try {
			if (str == null) i = 0;
			else             i = Integer.valueOf (new String (str)).intValue();
		} catch (java.lang.NumberFormatException e) {
			i = 0;
		}
		return i;
    }
	
    /**************************************************************************
	 * toDouble()
	 *   Return double created from a string.
	 **************************************************************************/
    private static double toDouble (String str) {
		double f;
		try {
			if (str == null) f = 0;
			else             f = Double.valueOf (str).doubleValue();
		} catch (java.lang.NumberFormatException e) {
			f = 0;
		}
		return f;
    }
	
}
