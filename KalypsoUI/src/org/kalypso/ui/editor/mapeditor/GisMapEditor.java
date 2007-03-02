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
package org.kalypso.ui.editor.mapeditor;

import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

/**
 * <p>
 * Eclipse-Editor zum editieren der GML-Gis-Templates.
 * </p>
 * 
 * @author belger, Stefan Kurzbach
 */
public class GisMapEditor extends AbstractMapPart implements IEditorPart
{
  public static final String ID = "org.kalypso.ui.editor.mapeditor.GisMapEditor";

  private GisMapOutlinePage m_gisMapOutlinePage;

  /**
   * @see org.kalypso.ui.editor.mapeditor.AbstractMapPart#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( IContentOutlinePage.class.equals( adapter ) )
    {
      if( m_gisMapOutlinePage == null )
      {
        m_gisMapOutlinePage = new GisMapOutlinePage( getCommandTarget() );
        m_gisMapOutlinePage.setMapModell( getMapPanel().getMapModell() );
      }
      return m_gisMapOutlinePage;
    }
    return super.getAdapter( adapter );
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.AbstractMapPart#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_gisMapOutlinePage != null )
    {
      m_gisMapOutlinePage.dispose();
    }
    super.dispose();
  }
}