/*******************************************************************************
 * CVRangeFactory
 * 
 * @author ETj
 */

package org.deegree_impl.model.cv;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.deegree.model.coverage.Level;
import org.deegree.services.RangeParamList;

public class CVRangeFactory
{
  //	private static HashMap names = new HashMap();
  //	private static HashMap tokens = new HashMap();
  //
  //	static
  //	{
  //		register(CVRange.class);
  //	}

  public static CVRange createRange( String name, String value, Level level, ArrayList subranges )
  {
    if( name.equals( CVRangeTime.RANGENAME ) )
      return new CVRangeTime( name, value, level, subranges );

    //		if(name.equals(CVRangeXXXX.RANGENAME))
    //			return new CVRangeXXXX(name, value, level, subranges);

    throw new IllegalArgumentException( "Unknown range type: " + name );
  }

  /**
   * Replaces rangeparam tokens with proper string FIXME Maybe this is the wrong
   * place for this method
   * 
   * @param rangeParams
   *          The instantiated request range paramenters
   * @param usedRanges
   *          The List of selected CVRanges
   * @param tileUrl
   *          The tile URL containing tokens to be replaces
   * 
   * @return The tileURL with replaced tokens
   */
  public static String substToken( RangeParamList rangeParams, List usedRanges, String tileUrl )
  {
    String ret = tileUrl;

    for( Iterator iter = usedRanges.iterator(); iter.hasNext(); )
    {
      CVRange range = (CVRange)iter.next();
      ret = range.substToken( rangeParams, ret );
    }

    //		if(tileUrl.indexOf("$") != -1)
    //			ret = CVRangeTime.substToken(rangeParams, ret);
    //
    //		if(tileUrl.indexOf("$") != -1)
    //			ret = CVRangexxx.substToken(rangeParams, ret);
    //
    //		// other kind of RangeParam ...

    return ret;
  }

  //	public static void register(Class range)
  //	{
  //		names.put(range.getName(), ;
  //	}

}

