package org.kalypso.ui.calcwizard.modelpages;

/**
 * @author belger
 */
public class TSLinkWithName
{
  public final String name;
  public final String linktype;
  public final String href;

  /**
   * Constructor
   * 
   * @param sname
   * @param slinktype
   * @param shref
   * @param filter
   */
  public TSLinkWithName( final String sname, final String slinktype, final String shref, final String filter )
  {
    this.name = sname;
    this.linktype = slinktype;
    
    if( filter != null && filter.length() > 0 )
      this.href = shref + "?" + filter;
    else
      this.href = shref;
  }
}
