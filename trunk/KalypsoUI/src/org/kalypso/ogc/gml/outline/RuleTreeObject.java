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
package org.kalypso.ogc.gml.outline;

import org.deegree.graphics.sld.Rule;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleCollection;

public class RuleTreeObject
{
  private Rule m_rule = null;

  private KalypsoUserStyle m_userStyle = null;

  private final IKalypsoFeatureTheme m_theme;

  public RuleTreeObject( final Rule rule, final KalypsoUserStyle style,
      final IKalypsoFeatureTheme theme )
  {
    m_rule = rule;
    m_userStyle = style;
    m_theme = theme;
  }

  public RuleTreeObject( final Object ruleObject, final KalypsoUserStyle style,
      final IKalypsoFeatureTheme theme )
  {
    // can be either a simple Rule or a collection of Pattern-Rules
    if( ruleObject instanceof Rule )
      m_rule = (Rule)ruleObject;
    // in case of pattern rules, just take the first rule for labeling the
    // outline view
    else if( ruleObject instanceof RuleCollection && ( (RuleCollection)ruleObject ).size() > 0 )
    {
      m_rule = ( (RuleCollection)ruleObject ).get( 0 );
    }
    m_userStyle = style;
    m_theme = theme;
  }

  public KalypsoUserStyle getStyle()
  {
    return m_userStyle;
  }

  public IKalypsoFeatureTheme getTheme()
  {
    return m_theme;
  }

  public Rule getRule()
  {
    return m_rule;
  }

  public String toString()
  {
    if( m_rule == null )
      return "<no styles set>";

    if( m_rule.getTitle() != null )
      return m_rule.getTitle();
    else if( m_rule.getName() != null )
      return m_rule.getName();
    else
      return m_rule.toString();
  }
}