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

import org.eclipse.jface.viewers.LabelProviderChangedEvent;
import org.eclipse.ui.internal.util.Util;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.ICheckStateProvider;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.ThemeStyleTreeObject;

/**
 * The this label provider modifies some labels for handling themes, that have only one style.
 * 
 * @author Gernot Belger
 */
@SuppressWarnings("restriction")
public class GisMapOutlineLabelProvider extends WorkbenchLabelProvider
{
  /**
   * If this parameter is set, the name of single styles of a theme is added to the theme name. For multiple styles of a
   * theme, this is not neccessary, because their level will be displayed in the outline then.
   */
  private final boolean m_showStyle;

  private final boolean m_showActive;

  /**
   * The constructor.
   * 
   * @param showStyle
   *            If this parameter is set, the name of single styles of a theme is added to the theme name. For multiple
   *            styles of a theme, this is not neccessary, because their level will be displayed in the outline then.
   */
  public GisMapOutlineLabelProvider( final boolean showStyle, final boolean showActive )
  {
    m_showStyle = showStyle;
    m_showActive = showActive;
  }

  public void elementsChanged( final Object... elements )
  {
    super.fireLabelProviderChanged( new LabelProviderChangedEvent( this, elements ) );
  }

  /**
   * @see org.eclipse.ui.model.WorkbenchLabelProvider#decorateText(java.lang.String, java.lang.Object)
   */
  @Override
  protected String decorateText( final String input, final Object element )
  {
    if( element instanceof IWorkbenchAdapter && element instanceof IKalypsoTheme )
    {
      final IWorkbenchAdapter adapter = (IWorkbenchAdapter) element;
      final IKalypsoTheme theme = (IKalypsoTheme) element;

      final boolean isActive = theme.getMapModell().isThemeActivated( theme );
      final String isActiveMsg = isActive && m_showActive ? "# " : "";

      /* Standard behaviour, if the style name for a single style of a theme should not be added. */
      if( m_showStyle == false )
        return isActiveMsg + input;

      final Object[] children = adapter.getChildren( element );
      if( !(children instanceof ThemeStyleTreeObject[]) )
        return isActiveMsg + input;

      if( children != null && (children.length == 0 || children.length > 1) )
        return isActiveMsg + input;

      final ThemeStyleTreeObject style = (ThemeStyleTreeObject) children[0];
      final String label = style.getLabel( style );

      if( label.contains( Messages.getString( "org.kalypso.ogc.gml.outline.GisMapOutlineLabelProvider.0" ) ) )
        return isActiveMsg + input;

      if( label.trim().equals( "" ) ) //$NON-NLS-1$
        return isActiveMsg + input;

      return isActiveMsg + input + " (" + label + ")"; //$NON-NLS-1$ //$NON-NLS-2$
    }

    return input;
  }

  public boolean isChecked( final Object element )
  {
    final ICheckStateProvider provider = getCheckStateProvider( element );
    if( provider == null )
      return false;

    return provider.isChecked();
  }

  public boolean isGrayed( final Object element )
  {
    final ICheckStateProvider provider = getCheckStateProvider( element );
    if( provider == null )
      return true;

    return provider.isGrayed();
  }

  private ICheckStateProvider getCheckStateProvider( final Object element )
  {
    return (ICheckStateProvider) Util.getAdapter( element, ICheckStateProvider.class );
  }

}