package org.kalypso.ui.calcwizard.modelpages;

/**
 * @author belger
 */
public class TSLinkWithName
{
  public final String name;
  public final String linktype;
  public final String href;

  public TSLinkWithName( final String name, final String linktype, final String href, final String filter )
  {
    this.name = name;
    this.linktype = linktype;
    
    if( filter != null && filter.length() > 0 )
      this.href = href + "?" + filter;
    else
      this.href = href;
  }
}
