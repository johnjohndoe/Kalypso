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

import java.util.HashMap;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
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

  private static final HashMap<String, RGB> m_markerTypes = new HashMap<String, RGB>();

  public DeviderProvider( )
  {
    m_markerTypes.put( IWspmTuhhConstants.MARKER_TYP_BORDVOLL, new RGB( 200, 50, 0 ) );
    m_markerTypes.put( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, new RGB( 0, 0, 255 ) );
    m_markerTypes.put( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, new RGB( 0, 180, 0 ) );
    m_markerTypes.put( IWspmTuhhConstants.MARKER_TYP_WEHR, new RGB( 0, 128, 0 ) );
  }

  /**
   * @deprecated
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#createProfilPointMarker(org.kalypso.observation.result.IComponent,
   *      org.kalypso.observation.result.IRecord)
   */

  @SuppressWarnings("deprecation")
  @Deprecated
  public IProfilPointMarker createProfilPointMarker( IComponent cmp, IRecord point )
  {
    /* first check, if provider provides markerType */
    if( !m_markerTypes.containsKey( cmp.getId() ) )
      throw new IllegalStateException( "ProfilPointMarkerProvider doesn't doesnt provides - " + cmp.getName() );
    /* point has component already defined? */
    if( !point.getOwner().hasComponent( cmp ) )
    {
      /* else create a new profile component */
      point.getOwner().addComponent( cmp );
    }

    /* create a new profile point marker */
    return new ProfilDevider( cmp, point );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#createProfilPointMarker(java.lang.String,
   *      org.kalypso.observation.result.IRecord)
   */
  @SuppressWarnings("deprecation")
  @Deprecated
  public IProfilPointMarker createProfilPointMarker( final String markerType, final IRecord point )
  {
    /* first check, if provider provides markerType */
    if( !m_markerTypes.containsKey( markerType ) )
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

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#getImageFor(java.lang.String)
   */
  public void drawMarker( final String[] markers, GC gc )
  {
// final ArrayList<RGB> rgbs = new ArrayList<RGB>(m_markerTypes.values());
// rgbs.add( 0, new RGB(255,255,255)) ;
// final ImageData imageData = background.getImageData();
    final int cnt = markers.length;
    final int offset = (16 - (3 * cnt)) / 2;
    int i = 0;
    final Color oldColor = gc.getBackground();
    for( final String marker : markers )
    {
      final Color color = new Color( gc.getDevice(), m_markerTypes.get( marker ) );
      try
      {
        gc.setBackground( color );
        gc.fillRectangle( offset + 4 * i++, 0, 3, 16 );
      }
      finally
      {
        gc.setBackground( oldColor );
        color.dispose();
      }

    }
    // return ImageDescriptor.createFromImageData( imageData );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#getColorFor(java.lang.String)
   */
  public RGB getColorFor( String marker )
  {
    return m_markerTypes.get( marker );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#getMarkers()
   */
  @SuppressWarnings("deprecation")
  public String[] getMarkerTypes( )
  {
    return m_markerTypes.keySet().toArray( new String[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#isMarker(org.kalypso.observation.result.IComponent)
   */
  @Deprecated
  @SuppressWarnings("deprecation")
  public boolean isMarker( final IComponent component )
  {
    return m_markerTypes.containsKey( component.getId() );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#providesPointMarker(org.kalypso.observation.result.IComponent)
   */
  @Deprecated
  @SuppressWarnings("deprecation")
  public boolean providesPointMarker( final IComponent marker )
  {
    try
    {
      IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( IWspmTuhhConstants.NS_WSPM_TUHH );
      return provider.providesPointProperty( marker.getId() );
    }
    catch( final IllegalStateException e )
    {
      return false;
    }
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider#getDefaultValue(java.lang.String)
   */
  public Object getDefaultValue( final String id )
  {
    // HACK: this is the only place where we use tuhh-stuff. Refaktor in order to reuse this code for other profile
    // types.
    if( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE.equals( id ) )
      return Boolean.TRUE;

    if( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE.equals( id ) )
      return "low";

    if( IWspmTuhhConstants.MARKER_TYP_BORDVOLL.equals( id ) )
      return Boolean.TRUE;

    if( IWspmTuhhConstants.MARKER_TYP_WEHR.equals( id ) )
      return new Double( 0.0 );

    return null;
  }

}
