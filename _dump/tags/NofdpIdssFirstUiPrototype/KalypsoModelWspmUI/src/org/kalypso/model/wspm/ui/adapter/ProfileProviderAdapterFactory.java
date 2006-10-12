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
package org.kalypso.model.wspm.ui.adapter;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter2;
import org.kalypso.model.wspm.ui.profil.view.IProfilProvider2;

/**
 * @author Gernot Belger
 */
public class ProfileProviderAdapterFactory implements IAdapterFactory
{
  private final Map<IWorkbenchPart, FeatureSelectionProfileProvider> m_providers = new HashMap<IWorkbenchPart, FeatureSelectionProfileProvider>();

  private IPartListener2 m_partAdapter = new PartAdapter2()
  { 
    /**
     * @see org.eclipse.ui.IPartListener2#partClosed(org.eclipse.ui.IWorkbenchPartReference)
     */
    @Override
    public void partClosed( final IWorkbenchPartReference partRef )
    {
      ProfileProviderAdapterFactory.this.partClosed( partRef.getPart( false ) );
    }
  };

  protected void partClosed( final IWorkbenchPart part )
  {
    synchronized( m_providers )
    {
      if( m_providers.containsKey( part ) )
      {
        final FeatureSelectionProfileProvider provider = m_providers.get( part );
        provider.dispose();
        m_providers.remove( part );
      }
    }
  }

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapter(java.lang.Object, java.lang.Class)
   */
  public Object getAdapter( final Object adaptableObject, final Class adapterType )
  {
    if( adapterType == IProfilProvider2.class )
    {
      if( adaptableObject instanceof IWorkbenchPart )
      {
        final IWorkbenchPart part = (IWorkbenchPart) adaptableObject;

        synchronized( m_providers )
        {
          if( m_providers.containsKey( part ) )
            return m_providers.get( part );

          final FeatureSelectionProfileProvider featureSelectionProfileProvider = new FeatureSelectionProfileProvider( part.getSite().getSelectionProvider() );

          m_providers.put( part, featureSelectionProfileProvider );
          part.getSite().getPage().addPartListener( m_partAdapter );

          return featureSelectionProfileProvider;
        }
      }
    }

    return null;
  }

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapterList()
   */
  public Class[] getAdapterList( )
  {
    return new Class[] { IProfilProvider2.class };
  }
}
