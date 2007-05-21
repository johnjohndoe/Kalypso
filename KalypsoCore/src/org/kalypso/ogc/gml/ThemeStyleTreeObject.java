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
 * Created on 22.07.2004
 * 
 * TODO To change the template for this generated file go to Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.ogc.gml;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.UserStyle;

public class ThemeStyleTreeObject implements IWorkbenchAdapter, ITooltipProvider
{
  private final KalypsoUserStyle m_style;

  private final IKalypsoFeatureTheme m_theme;

  public ThemeStyleTreeObject( final IKalypsoFeatureTheme theme, final UserStyle style )
  {
    m_theme = theme;
    m_style = (KalypsoUserStyle) style;
  }

  /**
   * @param theme
   * @param style
   */
  public ThemeStyleTreeObject( final IKalypsoFeatureTheme theme, final KalypsoUserStyle style )
  {
    m_theme = theme;
    m_style = style;
  }

  public KalypsoUserStyle getStyle( )
  {
    return m_style;
  }

  public IKalypsoFeatureTheme getTheme( )
  {
    return m_theme;
  }

  @Override
  public String toString( )
  {
    if( m_style == null )
    {
      return "<no styles set>";
    }

    if( m_style.getName() != null )
    {
      return m_style.getName();
    }
    return m_style.toString();
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object o )
  {
    if( o != this )
    {
      throw new IllegalStateException();
    }

    // TODO: is this right, always taking the first one?
    final FeatureTypeStyle[] styles = m_style.getFeatureTypeStyles();

    final Rule[] rules;
    if( styles.length > 0 )
    {
      rules = m_style.getFeatureTypeStyles()[0].getRules();
    }
    else
    {
      rules = new Rule[0];
    }

// // need to parse all rules as some might belong to a filter-rule-pattern
// RuleFilterCollection rulePatternCollection = RuleFilterCollection.getInstance();
// for( int i = 0; i < rules.length; i++ )
// {
// rulePatternCollection.addRule( rules[i] );
// }
// ArrayList filteredRules = rulePatternCollection.getFilteredRuleCollection();
// final RuleTreeObject[] result = new RuleTreeObject[filteredRules.size()];
// for( int i = 0; i < result.length; i++ )
// result[i] = new RuleTreeObject( filteredRules.get( i ), userStyle, (IKalypsoFeatureTheme) theme );
    final RuleTreeObject[] result = new RuleTreeObject[rules.length];
    for( int i = 0; i < rules.length; i++ )
    {
      result[i] = new RuleTreeObject( rules[i], this );
    }
    return result;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getImageDescriptor(java.lang.Object)
   */
  public ImageDescriptor getImageDescriptor( final Object object )
  {
    if( object != this )
    {
      throw new IllegalStateException();
    }

    final KalypsoUserStyle userStyle = getStyle();

    return userStyle.getImageDescriptor( userStyle );
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getLabel(java.lang.Object)
   */
  public String getLabel( final Object o )
  {
    if( o != this )
    {
      throw new IllegalStateException();
    }

    final KalypsoUserStyle userStyle = getStyle();

    return userStyle.getLabel( userStyle );
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getParent(java.lang.Object)
   */
  public Object getParent( final Object o )
  {
    if( o != this )
    {
      throw new IllegalStateException();
    }

    return getTheme();
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider#getTooltip(java.lang.Object)
   */
  public String getTooltip( final Object element )
  {
    if( element != this )
    {
      throw new IllegalStateException();
    }

    return getStyle().getAbstract();
  }
}