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

import java.util.ArrayList;

import org.deegree.graphics.sld.Rule;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleFilterCollection;

/**
 * Dieser TreeContentProvider akzeptiert nur MapModell'e als Input.
 * 
 * @author bce
 */
public class MapModellTreeContentProvider implements ITreeContentProvider, ModellEventListener
{
  protected Viewer m_viewer = null;

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object parentElement )
  {
    if( parentElement instanceof IKalypsoFeatureTheme )
    {
      final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme)parentElement;
      final UserStyle[] styles = theme.getStyles();
      if( styles != null )
      {
        final ThemeStyleTreeObject[] result = new ThemeStyleTreeObject[styles.length];
        for( int i = 0; i < styles.length; i++ )
          result[i] = new ThemeStyleTreeObject( theme, styles[i] );
        return result;
      }
    }
    else if( parentElement instanceof ThemeStyleTreeObject )
    {
      final ThemeStyleTreeObject obj = (ThemeStyleTreeObject)parentElement;

      final IKalypsoTheme theme = obj.getTheme();
      if( !( theme instanceof IKalypsoFeatureTheme ) )
        return null;

      final KalypsoUserStyle userStyle = obj.getStyle();
      final Rule[] rules = userStyle.getFeatureTypeStyles()[0].getRules();

      // need to parse all rules as some might belong to a filter-rule-pattern
      RuleFilterCollection rulePatternCollection = RuleFilterCollection.getInstance();
      for( int i = 0; i < rules.length; i++ )
      {
        rulePatternCollection.addRule( rules[i] );
      }
      ArrayList filteredRules = rulePatternCollection.getFilteredRuleCollection();
      final RuleTreeObject[] result = new RuleTreeObject[filteredRules.size()];
      for( int i = 0; i < result.length; i++ )
        result[i] = new RuleTreeObject( filteredRules.get( i ), userStyle,
            (IKalypsoFeatureTheme)theme );
      return result;
    }
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( final Object element )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( final Object element )
  {
    if( element instanceof ThemeStyleTreeObject )
    {
      ThemeStyleTreeObject obj = (ThemeStyleTreeObject)element;
      UserStyle userStyle = obj.getStyle();
      if( userStyle.getFeatureTypeStyles()[0].getRules().length > 0 )
        return true;
    }
    return ( element instanceof IKalypsoTheme );
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    final IMapModell mm = (IMapModell)inputElement;
    return mm == null ? null : mm.getAllThemes();
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose()
  {
    if( m_viewer != null )
    {
      final IMapModell input = (IMapModell)m_viewer.getInput();
      if( input != null )
        input.removeModellListener( this );
    }
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer,
   *            java.lang.Object, java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    if( oldInput != null )
      ( (IMapModell)oldInput ).removeModellListener( this );

    m_viewer = viewer;

    if( newInput != null )
      ( (IMapModell)newInput ).addModellListener( this );
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( m_viewer != null )
    {
      m_viewer.getControl().getDisplay().asyncExec( new Runnable()
      {

        public void run()
        {
          if( !m_viewer.getControl().isDisposed() )
            m_viewer.refresh();
        }
      } );
    }
  }
}