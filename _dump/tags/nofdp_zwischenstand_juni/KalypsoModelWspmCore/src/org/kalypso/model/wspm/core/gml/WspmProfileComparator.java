package org.kalypso.model.wspm.core.gml;

import java.util.Comparator;

public class WspmProfileComparator implements Comparator<WspmProfile>
{
  private final int m_factor;

  public WspmProfileComparator( final boolean isUpstream )
  {
    m_factor = isUpstream ? 1 : -1;
  }

  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( final WspmProfile o1, final WspmProfile o2 )
  {
    return m_factor * Double.compare( o1.getStation(), o2.getStation() );
  }
}