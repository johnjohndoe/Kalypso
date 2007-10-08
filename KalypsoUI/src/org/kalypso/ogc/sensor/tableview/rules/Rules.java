/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.tableview.rules;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

/**
 * Holds a list of rules.
 * 
 * @author schlienger
 */
public class Rules implements ITableViewRules
{
  private final List m_rules = new ArrayList();

  private final Map m_map = new HashMap();

  public Rules()
  {
  // empty
  }

  /**
   * Constructor with given rules
   * 
   * @param rules
   */
  public Rules( final RenderingRule[] rules )
  {
    m_rules.addAll( Arrays.asList( rules ) );
  }

  public void addRule( final RenderingRule rule )
  {
    m_rules.add( rule );
  }

  public void removeRule( final RenderingRule rule )
  {
    m_rules.remove( rule );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.rules.ITableViewRules#findRules(int)
   */
  public RenderingRule[] findRules( int mask )
  {
    return findRules( new Integer( mask ) );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.rules.ITableViewRules#findRules(java.lang.Integer)
   */
  public RenderingRule[] findRules( final Number mask ) throws NoSuchElementException
  {
    RenderingRule[] r = (RenderingRule[])m_map.get( mask );
    // TODO: the mapo is never reset, this smells buggy...
    if( r != null )
      return r;

    List lrules = new ArrayList();

    for( Iterator it = m_rules.iterator(); it.hasNext(); )
    {
      RenderingRule rule = (RenderingRule)it.next();

      if( rule.contains( mask.intValue() ) )
        lrules.add( rule );
    }

    r = (RenderingRule[])lrules.toArray( new RenderingRule[0] );
    m_map.put( mask, r );

    return r;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.rules.ITableViewRules#isEmpty()
   */
  public boolean isEmpty()
  {
    return m_rules.size() == 0;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.rules.ITableViewRules#getRules()
   */
  public List getRules()
  {
    return m_rules;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.rules.ITableViewRules#removeAllRules()
   */
  public void removeAllRules()
  {
    m_rules.clear();
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return "Rules (Amount= " + m_rules.size() + ")";
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.rules.ITableViewRules#cloneRules()
   */
  public ITableViewRules cloneRules()
  {
    final Rules rules = new Rules();

    for( Iterator iter = m_rules.iterator(); iter.hasNext(); )
    {
      final RenderingRule rule = (RenderingRule)iter.next();
      rules.addRule( rule.cloneRule() );
    }

    return rules;
  }
}