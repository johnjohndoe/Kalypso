/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.ui.views.properties.IPropertySheetPage;
import org.eclipse.ui.views.properties.tabbed.ITabbedPropertySheetPageContributor;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;

/**
 * Generel adapter factory for {@link AbstractMapPart}.
 * <p>
 * Some adapterTypes (like {@link MapPanel}) are exposed via this factory (instead of directly using
 * {@link AbstractMapPart#getAdapter(Class)}, because the expression frameworks 'adapt'-element does only recognise
 * types handled via adapter factories.
 * 
 * @author Gernot Belger
 */
public class MapPartAdapterFactory implements IAdapterFactory
{
  private static final String CONTENT_OUTLINE_PROPERTY_CONTRIBUTOR = "org.kalypso.ui.ContentOutlinePropertyContributor"; //$NON-NLS-1$

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapter(java.lang.Object, java.lang.Class)
   */
  @SuppressWarnings("unchecked")//$NON-NLS-1$
  public Object getAdapter( final Object adaptableObject, final Class adapterType )
  {
    if( !(adaptableObject instanceof AbstractMapPart) )
      return null;

    final AbstractMapPart mapPart = (AbstractMapPart) adaptableObject;

    if( adapterType == IPropertySheetPage.class )
      return new TabbedPropertySheetPage( new ITabbedPropertySheetPageContributor()
      {
        public String getContributorId( )
        {
          return MapPartAdapterFactory.CONTENT_OUTLINE_PROPERTY_CONTRIBUTOR;
        }
      } );

    if( adapterType == MapPanel.class )
      return mapPart.getMapPanel();

    return null;
  }

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapterList()
   */
  @SuppressWarnings("unchecked")//$NON-NLS-1$
  public Class[] getAdapterList( )
  {
    return new Class[] { IPropertySheetPage.class, MapPanel.class };
  }

}
