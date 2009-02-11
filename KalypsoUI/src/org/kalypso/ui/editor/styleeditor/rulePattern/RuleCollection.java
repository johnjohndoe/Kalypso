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
/*
 * Created on 12.09.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.rulePattern;

import java.util.ArrayList;

import org.kalypsodeegree.graphics.sld.Rule;

/**
 * @author F.Lindemann
 */
public class RuleCollection
{
  private final ArrayList<Rule> m_rules = new ArrayList<Rule>();

  private final String m_id;

  private RuleCollection( final Rule rule )
  {
    m_id = rule.getTitle();
    m_rules.add( rule );
  }

  public static RuleCollection getInstance( Rule rule )
  {
    return new RuleCollection( rule );
  }

  public void addRule( Rule rule )
  {
    m_rules.add( rule );
  }

  public void removeRule( Rule rule )
  {
    m_rules.remove( rule );
  }

  public String getId( )
  {
    return m_id;
  }

  public Rule get( final int index )
  {
    return m_rules.get( index );
  }

  public int size( )
  {
    return m_rules.size();
  }
}