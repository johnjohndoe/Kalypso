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
package org.kalypso.model.wspm.tuhh.core.profile;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;

/**
 * @author kimwerner
 */
public class DeviderProvider implements IProfilPointMarkerProvider
{
  private static final String[] m_markerTypes = { IWspmTuhhConstants.MARKER_TYP_BORDVOLL, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE,
      IWspmTuhhConstants.MARKER_TYP_WEHR };

  
  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#getImageFor(java.lang.String)
   */
  public ImageDescriptor getImageFor( String markerId )
  {
    final String plgn = KalypsoModelWspmTuhhCorePlugin.getDefault().toString();
    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( markerId ) )
      return AbstractUIPlugin.imageDescriptorFromPlugin( plgn, "icons/obj16/legend_col1.gif" );
    if( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE.equals( markerId ) )
      return AbstractUIPlugin.imageDescriptorFromPlugin( plgn, "icons/obj16/legend_col2.gif" );
    if( IWspmTuhhConstants.MARKER_TYP_BORDVOLL.equals( markerId ) )
      return AbstractUIPlugin.imageDescriptorFromPlugin( plgn, "icons/obj16/legend_col3.gif" );
    if( IWspmTuhhConstants.MARKER_TYP_WEHR.equals( markerId ) )
      return AbstractUIPlugin.imageDescriptorFromPlugin( plgn, "icons/obj16/legend_col4.gif" );
    return null;
  }

 
 
  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#createMarker(java.lang.String)
   */
  public IProfilPointMarker createMarker( String markerId )
  {
    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( markerId ) )
      return new ProfilDevider( markerId, "Trennfläche", new String[] { IWspmTuhhConstants.POINTMARKER_PROPERTY_BOESCHUNG, IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT } );
    if( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE.equals( markerId ) )
      return new ProfilDevider( markerId, "Durchströmter Bereich", new String[] { IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT } );
    if( IWspmTuhhConstants.MARKER_TYP_BORDVOLL.equals( markerId ) )
      return new ProfilDevider( markerId, "Bordvollpunkt", new String[0] );
    if( IWspmTuhhConstants.MARKER_TYP_WEHR.equals( markerId ) )
      return new ProfilDevider( markerId, "Wehrbereichstrenner", new String[] { IWspmTuhhConstants.POINTMARKER_PROPERTY_BEIWERT } );
    return null;

  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#getMarkerTypes()
   */
  public String[] getMarkerTypes( )
  {
    return m_markerTypes;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#providesPointMarker(java.lang.String)
   */
  public boolean providesPointMarker( String markerId )
  {
    return IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( markerId ) || IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE.equals( markerId ) || IWspmTuhhConstants.MARKER_TYP_BORDVOLL.equals( markerId )
        || IWspmTuhhConstants.MARKER_TYP_WEHR.equals( markerId );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#createMarkerFromGml(java.lang.String,
   *      java.lang.Object)
   */
  public IProfilPointMarker createMarkerFromGml( final String markerId, final Object value )
  {
    // Devider
    if( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE.equals( markerId ) )
    {
      final Boolean hasDevider = (Boolean) value;
      if( hasDevider != null && hasDevider.booleanValue() )
        return createMarker( markerId );

      return null;
    }

    if( IWspmTuhhConstants.MARKER_TYP_BORDVOLL.equals( markerId ) )
    {
      final Boolean hasDevider = (Boolean) value;
      if( hasDevider != null && hasDevider.booleanValue() )
        return createMarker( markerId );

      return null;
    }

    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( markerId ) )
    {
      final String kind = (String) value;
      if( "low".equals( kind ) || "high".equals( kind ) )
      {
        final IProfilPointMarker marker = createMarker( markerId );
        final Boolean high = Boolean.valueOf( "high".equals( kind ) );
        marker.setValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_BOESCHUNG, high );
        return marker;
      }

      return null;
    }
    else if( IWspmTuhhConstants.MARKER_TYP_WEHR.equals( markerId ) )
    {
      if( value == null )
        return null;

      final IProfilPointMarker marker = createMarker( markerId );
      marker.setValueFor( IWspmTuhhConstants.POINTMARKER_PROPERTY_BEIWERT, value );
      return marker;
    }

    return null;
  }

}
