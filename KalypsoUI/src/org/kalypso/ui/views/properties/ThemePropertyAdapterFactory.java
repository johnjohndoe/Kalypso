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
import org.eclipse.ui.model.IWorkbenchAdapter2;
import org.eclipse.ui.views.properties.IPropertySource;
import org.eclipse.ui.views.properties.tabbed.ITabbedPropertySheetPageContributor;
import org.kalypso.ogc.gml.IKalypsoTheme;

/**
 * @author Gernot Belger
 */
public class ThemePropertyAdapterFactory implements IAdapterFactory
{
  private static final String ORG_KALYPSO_UI_THEME_PROPERTY_CONTRIBUTOR = "org.kalypso.ui.ThemePropertyContributor"; //$NON-NLS-1$

  private static final ITabbedPropertySheetPageContributor THE_THEME_CONTRIBUTOR = new ITabbedPropertySheetPageContributor()
  {
    public String getContributorId( )
    {
      return ORG_KALYPSO_UI_THEME_PROPERTY_CONTRIBUTOR;
    }
  };

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapter(java.lang.Object, java.lang.Class)
   */
  @SuppressWarnings("unchecked") //$NON-NLS-1$
  public Object getAdapter( final Object adaptableObject, final Class adapterType )
  {
    if( adapterType == ITabbedPropertySheetPageContributor.class )
      return THE_THEME_CONTRIBUTOR;

    if( adapterType == IPropertySource.class )
      return new ThemePropertySource( (IKalypsoTheme) adaptableObject );

    if( adapterType == IWorkbenchAdapter2.class )
      return new ThemeWorkbenchAdapter( (IKalypsoTheme) adaptableObject );

    return null;
  }

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapterList()
   */
  @SuppressWarnings("unchecked") //$NON-NLS-1$
  public Class[] getAdapterList( )
  {
    return new Class[] { ITabbedPropertySheetPageContributor.class };
  }

}
