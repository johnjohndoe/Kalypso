/*******************************************************************************
 * CVRange.java
 * 
 * @author ETj
 */

package org.deegree_impl.model.cv;

import java.util.ArrayList;
import java.util.Calendar;

import org.deegree.model.coverage.Level;
import org.deegree.services.RangeParamList;
import org.deegree.services.TimeExtent;
import org.deegree_impl.services.RangeParam;
import org.deegree_impl.services.RangeParamTime;
import org.deegree_impl.services.TimeExtent_Impl;
import org.deegree_impl.tools.StringExtend;

/*
 *  
 */

public class CVRangeTime extends CVRange
{
  public static final String RANGENAME = "time";

  public String getRangeName()
  {
    return RANGENAME;
  }

  protected TimeExtent _te;

  public CVRangeTime( String name, String value, Level level, ArrayList subranges )
  {
    super( name, value, level, subranges );
  }

  protected void setValue( String value )
  {
    _te = new TimeExtent_Impl( value );
  }

  /** @return a TimeExtent */
  public Object getValue()
  {
    return _te;
  }

  /**
   * Check if a range parameter match this CVRange FIXME this method needs to be
   * expanded a lot...
   * 
   * @param param
   *          the TimeExtent to be compared against this CVRangeTime
   * 
   * @return true if <I>te </I> is enclosed in this RangeTime
   */
  public boolean match( RangeParam param )
  {
    if( !param.getName().equalsIgnoreCase( RANGENAME ) )
      throw new IllegalArgumentException( "RangeTime can only be matched against RangeParamTime" );

    TimeExtent te = (TimeExtent)param.getValue();

    if( !te.isSingle() )
      throw new IllegalArgumentException( "Time RangeParam can only be a single date." );

    Calendar date = te.getDate();

    if( _te.isSingle() )
    {
      return _te.getDate().equals( date );
    }

    if( _te.isMulti() )
    {
      for( int i = 0; i < _te.getListLength(); i++ )
      {
        Calendar c = _te.getDate( i );
        if( c.equals( date ) )
          return true;
      }
    }

    if( _te.isPeriodic() )
    {
      if( date.equals( _te.getStartDate() ) || date.equals( _te.getEndDate() ) )
        return true;

      if( date.after( _te.getStartDate() ) && date.before( _te.getEndDate() ) )
        return true;
    }

    return false;
  }

  /**
   * Replaces the known tokens with values instantiated in the proper RangeParam
   * 
   * @param rpl
   *          a RangeParamList
   * @param stringWithTokens
   *          a String
   * 
   * @return a String with known tokens replaced
   *  
   */
  public String substToken( RangeParamList rpl, String stringWithTokens )
  {
    RangeParamTime rpt = (RangeParamTime)rpl.getParameter( RANGENAME );
    Calendar base = null;

    if( rpt == null )
    {
      System.out.println( "No time parameter. Using default one." );
      System.out.println( "PARAMS: " + rpl );

      base = Calendar.getInstance();
    }
    else
    {
      System.out.println( "Time parameter is " + rpt );
      System.out.println( "Time extent is " + rpt.getValue() );

      TimeExtent te = (TimeExtent)rpt.getValue();
      if( !te.isSingle() )
        throw new IllegalArgumentException( "Time RangeParam can only replace single date." );

      base = te.getDate();
    }

    return doSubst( base, stringWithTokens );
  }

  /**
   * Method doSubst
   * 
   * @param base
   *          a Calendar
   * @param stringWithTokens
   *          a String
   * 
   * @return a String
   */
  protected String doSubst( Calendar base, String stringWithTokens )
  {
    if( stringWithTokens.indexOf( "$" ) == -1 ) // speedup
      return stringWithTokens;

    String syear = "" + base.get( Calendar.YEAR );
    String smonth = pad0( base.get( Calendar.MONTH ) + 1 );
    String sday = pad0( base.get( Calendar.DAY_OF_MONTH ) );

    if( _te.isPeriodic() && _te.getPeriod().isDecadic() )
    {
      int iday = base.get( Calendar.DAY_OF_MONTH );
      if( iday < 10 )
        iday = 1;
      else if( iday < 20 )
        iday = 10;
      else
        iday = 20;
      sday = pad0( iday );
    }

    String ret = StringExtend.replace( stringWithTokens, "$YEAR", syear, true );
    ret = StringExtend.replace( ret, "$MONTH", smonth, true );
    ret = StringExtend.replace( ret, "$DAY", sday, true );

    //		if(ret.indexOf("$") == -1) // speedup
    //			return ret;

    String shour = pad0( base.get( Calendar.HOUR_OF_DAY ) );
    String smin = pad0( base.get( Calendar.MINUTE ) );
    String ssec = pad0( base.get( Calendar.SECOND ) );

    ret = StringExtend.replace( ret, "$HOUR", shour, true );
    ret = StringExtend.replace( ret, "$MINUTE", smin, true );
    ret = StringExtend.replace( ret, "$SECOND", ssec, true );

    return ret;
  }

  private static String pad0( int value )
  {
    return ( value > 9 ) ? "" + value : "0" + value;
  }

}

