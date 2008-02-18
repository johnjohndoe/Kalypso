/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.outline;

import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.kalypso.ogc.gml.AbstractKalypsoTheme;
import org.kalypso.ogc.gml.RuleTreeObject;
import org.kalypso.ogc.gml.ThemeStyleTreeObject;

/**
 * Content provider for modifying the outline tree. It filters the styles and the rules.
 * 
 * @author Holger Albert
 */
public class GisMapOutlineContentProvider extends WorkbenchContentProvider
{
  /**
   * The constructor.
   */
  public GisMapOutlineContentProvider( )
  {
  }

  /**
   * @see org.eclipse.ui.model.BaseWorkbenchContentProvider#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( Object element )
  {
    /* A moment please, if the theme is configured, not to show its children, then ignore them. */
    if( element instanceof AbstractKalypsoTheme )
    {
      AbstractKalypsoTheme theme = (AbstractKalypsoTheme) element;
      if( theme.shouldShowChildren() == false )
        return new Object[] {};
    }

    Object[] children = super.getChildren( element );

    /* If no childs are there, return the result. */
    if( children == null || children.length == 0 )
      return children;

    /* If more then one child are there, return the result. */
    if( children.length > 1 )
      return children;

    /* Now there is only one child. If it is not a style or a rule, return the result. */
    if( !(children instanceof ThemeStyleTreeObject[]) && !(children instanceof RuleTreeObject[]) )
      return children;

    /* It has to be a IWorkbenchAdapter. */
    IWorkbenchAdapter child = (IWorkbenchAdapter) children[0];

    /* Get the children of the child. */
    Object[] children2 = super.getChildren( child );

    /* If there are more then one as a result, return them instead. */
    if( children2 != null && children2.length > 1 )
      return children2;

    /* Otherwise ignore it. */
    return new Object[] {};
  }

  /**
   * @see org.eclipse.ui.model.BaseWorkbenchContentProvider#getParent(java.lang.Object)
   */
  @Override
  public Object getParent( Object element )
  {
    /* Only rules may jump above parents. */
    if( !(element instanceof RuleTreeObject) )
      return super.getParent( element );

    /* Get the parent for the rule (this would be the style). */
    Object style = super.getParent( element );

    /* Get the parent of the style (this would be a theme). */
    Object theme = super.getParent( style );

    /* Get the childs of the theme (this would be all styles). */
    Object[] styles = getChildren( theme );

    /* Check, if there are more than one style. If so, return the normal result. */
    if( styles.length > 1 )
      return style;

    /* Otherwise, there is only one style. */
    return theme;
  }
}