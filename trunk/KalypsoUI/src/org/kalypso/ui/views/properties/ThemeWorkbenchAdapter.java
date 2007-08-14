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
package org.kalypso.ui.views.properties;

import org.eclipse.jface.resource.FontDescriptor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.ui.model.WorkbenchAdapter;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

/**
 * Only the {@link org.eclipse.ui.model.IWorkbenchAdapter2} part is implemented here.
 * <p>
 * Everything what concerns the {@link org.eclipse.ui.model.IWorkbenchAdapter} interface is directly implemented by the
 * {@link IKalypsoTheme} implementors.
 * </p>
 * 
 * @author Gernot Belger
 */
public class ThemeWorkbenchAdapter extends WorkbenchAdapter
{
  private final IKalypsoTheme m_theme;

  public ThemeWorkbenchAdapter( final IKalypsoTheme theme )
  {
    m_theme = theme;
  }

  /**
   * Not loaded themes are shown with italic font.
   * 
   * @see org.eclipse.ui.model.WorkbenchAdapter#getFont(java.lang.Object)
   */
  @Override
  public FontData getFont( final Object element )
  {
    System.out.print( "Font for: " + element );

    final FontData defaultFont = super.getFont( element );
    final FontData standardFont = defaultFont == null ? JFaceResources.getDialogFont().getFontData()[0] : defaultFont;

    FontDescriptor fontDesc = FontDescriptor.createFrom( standardFont );

    if( !m_theme.isLoaded() )
      fontDesc = fontDesc.setStyle( SWT.ITALIC );

    // falls aktiviert
    final IMapModell mapModell = m_theme.getMapModell();
    if( mapModell != null && mapModell.getActiveTheme() == m_theme )
    {
      fontDesc = fontDesc.setStyle( SWT.BOLD );
      System.out.print( ": BOLD" );
    }

    System.out.println();

    return fontDesc.getFontData()[0];
  }

}
