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
package org.kalypso.model.wspm.tuhh.core.profile;

import java.util.Arrays;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * TODO - IPointPropertyProvider and IMarkerProvider should be the same Provider Class - in TupleResult view, their is
 * no difference between them - only column name (type) defines markers
 * 
 * @author kimwerner
 */
public class DeviderProvider implements IProfilPointMarkerProvider
{
  private static final String[] m_markerTypes = { IWspmTuhhConstants.MARKER_TYP_BORDVOLL, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE,
      IWspmTuhhConstants.MARKER_TYP_WEHR };

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#getImageFor(java.lang.String)
   */
  public ImageDescriptor getImageFor( final String markerId )
  {
    final String plgn = KalypsoModelWspmTuhhCorePlugin.getDefault().toString();
    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( markerId ) )
    {
      final ImageData imageData = new ImageData( 16, 16, 2, new PaletteData( new RGB[] { new RGB( 255, 255, 255 ),IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE_COLOR , IWspmTuhhConstants.MARKER_TYP_BORDVOLL_COLOR  } ) );// {IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE_COLOR
                                                                                                                        // } )
                                                                                                                        // ) );
      byte[] values = new byte[4];
      // imageData.getPixels( 0, 0, 8, values, 0 ) ;

      Arrays.fill( values, (byte) 1);
      for( int i = 0; i < 16; i++ )
        imageData.setPixels( 4, i, 4, values, 0 );
      Arrays.fill( values, (byte) 2);
      for( int i = 0; i < 16; i++ )
        imageData.setPixels( 8, i, 4, values, 0 );
      return ImageDescriptor.createFromImageData( imageData );

      // return ImageDescriptor.createFromImageData( new ImageData( 16, 16, 1, new PaletteData( new RGB[]{})));//
      // {IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE_COLOR } ) ) );
      // return AbstractUIPlugin.imageDescriptorFromPlugin( plgn, "icons/obj16/legend_col4.gif" );
    }
    if( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE.equals( markerId ) )
      return ImageDescriptor.createFromImageData( new ImageData( 16, 16, 1, new PaletteData( new RGB[] { IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE_COLOR } ) ) );
    if( IWspmTuhhConstants.MARKER_TYP_BORDVOLL.equals( markerId ) )
      return ImageDescriptor.createFromImageData( new ImageData( 16, 16, 1, new PaletteData( new RGB[] { IWspmTuhhConstants.MARKER_TYP_BORDVOLL_COLOR } ) ) );
    if( IWspmTuhhConstants.MARKER_TYP_WEHR.equals( markerId ) )
      return ImageDescriptor.createFromImageData( new ImageData( 16, 16, 1, new PaletteData( new RGB[] { IWspmTuhhConstants.MARKER_TYP_WEHR_COLOR } ) ) );
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#getMarkers()
   */
  public String[] getMarkerTypes( )
  {
    return m_markerTypes;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#isMarker(org.kalypso.observation.result.IComponent)
   */
  public boolean isMarker( final IComponent component )
  {
    return ArrayUtils.contains( m_markerTypes, component.getId() );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#providesPointMarker(org.kalypso.observation.result.IComponent)
   */
  public boolean providesPointMarker( final IComponent marker )
  {
    try
    {
      PointPropertyProviderTUHH.createPointProperty( marker.getId() );
      return true;
    }
    catch( final IllegalStateException e )
    {
      return false;
    }
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#createProfilPointMarker(java.lang.String,
   *      org.kalypso.observation.result.IRecord)
   */
  public IProfilPointMarker createProfilPointMarker( final String markerType, final IRecord point )
  {
    /* first check, if provider provides markerType */
    if( !ArrayUtils.contains( m_markerTypes, markerType ) )
      throw new IllegalStateException( "ProfilPointMarkerProvider doesn't doesnt provides - " + markerType );

    /* point has component already defined? */
    final IComponent[] components = point.getOwner().getComponents();
    IComponent comp = ProfilObsHelper.getComponentById( components, markerType );
    if( comp == null )
    {
      /* create a new profile component */
      comp = PointPropertyProviderTUHH.createPointProperty( markerType );
      point.getOwner().addComponent( comp );
    }

    /* create a new profile point marker */
    return new ProfilDevider( comp, point );
  }

}
