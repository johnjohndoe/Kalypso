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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.impl.ProfilEventManager;
import org.kalypso.model.wspm.ui.action.FeatureComparator;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import com.vividsolutions.jts.geom.LineString;

/**
 * State object for create main channel widget and composite.
 * 
 * @author Thomas Jung
 */
public class CreateChannelData
{
  public enum SIDE
  {
    LEFT,
    RIGHT;
  }

  public enum PROF
  {
    UP,
    DOWN;
  }
  
  private IKalypsoFeatureTheme m_profileTheme;

  private IKalypsoFeatureTheme m_bankTheme;

  private Set<Feature> m_selectedProfiles = new HashSet<Feature>();
  
  private int m_numbProfileIntersections;

  private Map<Feature, SIDE> m_selectedBanks = new HashMap<Feature, SIDE>();

  private final CreateMainChannelWidget m_widget;

  private boolean datacomplete;

  private int m_numBankSelections;
  
  private int m_globNumbBankIntersections;

  private List<SegmentData> m_segmentList = new LinkedList<SegmentData>();

  public CreateChannelData( final CreateMainChannelWidget widget )
  {
    m_widget = widget;
  }

  /* --------------------- Theme handling ---------------------------------- */

  public IKalypsoFeatureTheme getProfileTheme( )
  {
    return m_profileTheme;
  }

  public IKalypsoFeatureTheme getBankTheme( )
  {
    return m_bankTheme;
  }

  public void setProfileTheme( final IKalypsoFeatureTheme profileTheme )
  {
    m_profileTheme = profileTheme;

    // TODO: event handling
  }

  public void setBankTheme( final IKalypsoFeatureTheme bankTheme )
  {
    m_bankTheme = bankTheme;

    // TODO: event handling
  }

  /**
   * Gets the WSPM profile themes in the Kalypso theme list
   */
  public IKalypsoFeatureTheme[] getProfileThemes( )
  {
    final IMapModell mapModell = m_widget.getPanel().getMapModell();
    if( mapModell == null )
      return new IKalypsoFeatureTheme[0];

    final IKalypsoTheme[] allThemes = mapModell.getAllThemes();

    final List<IKalypsoFeatureTheme> goodThemes = new ArrayList<IKalypsoFeatureTheme>();

    for( final IKalypsoTheme theme : allThemes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme fTheme = (IKalypsoFeatureTheme) theme;
        final IFeatureType featureType = fTheme.getFeatureType();

        if( featureType != null && GMLSchemaUtilities.substitutes( featureType, WspmProfile.QNAME_PROFILE ) )
          goodThemes.add( fTheme );
      }
    }

    return goodThemes.toArray( new IKalypsoFeatureTheme[goodThemes.size()] );
  }

  /**
   * Gets the linestring themes in the Kalypso theme list
   */
  public IKalypsoFeatureTheme[] getBankThemes( )
  {
    final IKalypsoTheme[] allThemes = m_widget.getPanel().getMapModell().getAllThemes();

    final List<IKalypsoFeatureTheme> goodThemes = new ArrayList<IKalypsoFeatureTheme>();

    for( final IKalypsoTheme theme : allThemes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme fTheme = (IKalypsoFeatureTheme) theme;
        final IFeatureType featureType = fTheme.getFeatureType();

        if( featureType == null )
          return new IKalypsoFeatureTheme[0];
        final IValuePropertyType[] allGeomteryProperties = featureType.getAllGeomteryProperties();
        // choose only the linestrings
        // just take the first found property
        if( allGeomteryProperties.length > 0 && allGeomteryProperties[0].getValueQName().equals( GeometryUtilities.QN_LINE_STRING_PROPERTY ) )
          goodThemes.add( fTheme );
      }
    }
    return goodThemes.toArray( new IKalypsoFeatureTheme[goodThemes.size()] );
  }

  /* --------------------- selection handling ---------------------------------- */

  public void addSelectedProfiles( final Feature[] profileFeatures )
  {
    m_selectedProfiles.addAll( Arrays.asList( profileFeatures ) );

    m_widget.update();
  }

  public void removeSelectedProfiles( final Feature[] profileFeatures )
  {
    m_selectedProfiles.removeAll( Arrays.asList( profileFeatures ) );

    m_widget.update();
  }

  public Feature[] getSelectedProfiles( )
  {
    return m_selectedProfiles.toArray( new Feature[m_selectedProfiles.size()] );
  }

  /**
   * @param side
   *          false: left, true: right
   */
  public void addSelectedBanks( final Feature[] bankFeatures, final SIDE side )
  {
    for( final Feature feature : bankFeatures )
      m_selectedBanks.put( feature, side );

    m_widget.update();
  }

  public Feature[] getSelectedBanks( final SIDE side )
  {
    final List<Feature> result = new ArrayList<Feature>();

    for( final Map.Entry<Feature, SIDE> entry : m_selectedBanks.entrySet() )
    {
      final Feature bankFeature = entry.getKey();
      final SIDE value = entry.getValue();
      if( value == side )
        result.add( bankFeature );
    }

    return result.toArray( new Feature[result.size()] );
  }

  public void removeSelectedBanks( final Feature[] bankFeatures )
  {
    for( final Feature feature : bankFeatures )
      m_selectedBanks.remove( feature );

    m_widget.update();

    m_selectedBanks.keySet().removeAll( Arrays.asList( bankFeatures ) );
    intersectBanksWithProfs();

    m_widget.update();
  }

  /* --------------------- profile chart handling ---------------------------------- */

  public IProfilEventManager getProfilEventManager( )
  {
    if( m_selectedProfiles.size() == 0 )
      return new ProfilEventManager( null, null );

    try
    {
      final Feature profileFeature = m_selectedProfiles.iterator().next();
      final IProfil profil = ProfileFeatureFactory.toProfile( profileFeature );
      final ProfilEventManager pem = new ProfilEventManager( profil, null );
      return pem;
    }
    catch( final ProfilDataException e )
    {
      e.printStackTrace();
    }
    return new ProfilEventManager( null, null );
  }

  /* --------------------- workflow handling ---------------------------------- */

  /**
   * check, if all necessary data is specified (profiles, bank lines...).
   */
  public void completationCheck( )
  {

    // TODO: check for left and right banks and the selected profiles
    datacomplete = false;
    if( m_selectedBanks.size() > 0 && m_selectedProfiles.size() > 1 )
    {
        datacomplete = true;
    }
    else if( m_selectedProfiles.size() <= 1 )
    {
      m_segmentList.clear();
    }
    if( datacomplete == true )
      /* intersects the banks with the profiles and manages the segment creation */
      intersectBanksWithProfs();
    if (m_segmentList.size()> 0)
      /* if there are segments, start Quadmesher */
      manageQuadMesher();
  }

  /**
   * this is the manager for starting the JTSQuadMesher for all complete segments
   */
  private void manageQuadMesher( )
  {
    for( int i = 0; i < m_segmentList.size(); i++ )
    {
      final boolean complete;
      SegmentData segment = m_segmentList.get( i );
      complete = segment.complete();
      if (complete == true )
      {
        /* arrange the for lines for the mesher */
        
        
      }
      
      
      
    }
    
  }

  /**
   * all selected banks will be intersected by the selected profiles Input: all selected banks and profiles Output:
   * intersected banks as linestrings (including the intersection point)
   */
  private void intersectBanksWithProfs( )
  {
    /* at first -> clear the segment list! */
    m_segmentList.clear();
    

    final Feature[] profileFeatures = m_selectedProfiles.toArray( new Feature[m_selectedProfiles.size()] );

    if( profileFeatures.length == 0 )
      return;

    final IPropertyType stationProperty = profileFeatures[0].getFeatureType().getProperty( WspmProfile.QNAME_STATION );
    Arrays.sort( profileFeatures, new FeatureComparator( stationProperty ) );

    // TODO: intersect all selected profiles and banks and store it into a linkedList of SegmentData.

    // loop over all profiles
    // take two neighbouring profiles create a segment for them

    WspmProfile lastProfile = null;
    for( final Feature profileFeature : profileFeatures )
    {

      // get the profile line
      final WspmProfile profile = new WspmProfile( profileFeature );
      final GM_Curve profCurve = profile.getLine();

      if( lastProfile != null )
      {
        // behandle das segment
        final int numBankIntersections = getGlobNumBankIntersections(); 
        final SegmentData segment = new SegmentData( this, lastProfile, profile, m_selectedBanks, numBankIntersections );
        System.out.println( "Last profile: " + lastProfile.getStation() );
        System.out.println( "Profile: " + profile.getStation() );
        
        // add to list
        m_segmentList.add( segment );   
      }
      else
      {
        // tu nix
      }
      lastProfile = profile;
    }
  }

  private void calculateSegment( int segNumB )
  {

  }

  private void calculateAllSegment( )
  {

  }

  public void paintAllSegments( final Graphics g, final MapPanel mapPanel )
  {
    // loop over all segments
    final SegmentData[] datas = m_segmentList.toArray( new SegmentData[m_segmentList.size()] );
    for( final SegmentData segment : datas )
    {
      if( segment != null )
        try
        {
          segment.paintSegment( g, mapPanel );
        }
        catch( GM_Exception e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
    }

  }
  
  public void setNumProfileIntersections ( int numProfileIntersections)
  {
    m_numbProfileIntersections = numProfileIntersections;
    completationCheck();
  }

  public int getNumProfileIntersections ()
  {
    final int numProfileIntersections = m_numbProfileIntersections;
    return numProfileIntersections;
  }
  
  public void setGlobNumBankIntersections ( int globNumBankIntersections)
  {
    m_globNumbBankIntersections = globNumBankIntersections;
    completationCheck();
  }
  
  public int getGlobNumBankIntersections ()
  {
    final int globNumBankIntersections = m_globNumbBankIntersections;
    return globNumBankIntersections;
  }
  
}
