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
package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.deegree.graphics.displayelements.DisplayElement;
import org.deegree.graphics.displayelements.IncompatibleGeometryTypeException;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureList;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.graphics.displayelements.DisplayElementFactory;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.sort.SplitSort;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.util.command.ICommand;

/**
 * @author vdoemming
 */
public class KalypsoFeatureTheme extends AbstractKalypsoTheme implements IKalypsoFeatureTheme
{
  CommandableWorkspace m_workspace;

  private final HashMap m_styleDisplayMap = new HashMap();

  private final FeatureType m_featureType;

  final FeatureList m_featureList;

  public KalypsoFeatureTheme( final CommandableWorkspace workspace, final String featurePath,
      final String name )
  {
    super( name );

    m_workspace = workspace;

    final Object featureFromPath = workspace.getFeatureFromPath( featurePath );
    if( featureFromPath instanceof FeatureList )
    {
      m_featureType = workspace.getFeatureTypeFromPath( featurePath );
      m_featureList = (FeatureList)featureFromPath;
    }
    else if( featureFromPath instanceof Feature )
    {
      m_featureList = new SplitSort();
      m_featureList.add( featureFromPath );
      m_featureType = ( (Feature)featureFromPath ).getFeatureType();
    }
    else
      throw new IllegalArgumentException( "FeaturePath doesn't point to feature collection: "
          + featurePath );
    m_workspace.addModellListener( this );
  }

  public void dispose()
  {
    final Set set = m_styleDisplayMap.keySet();
    final KalypsoUserStyle[] styles = (KalypsoUserStyle[])set.toArray( new KalypsoUserStyle[set
        .size()] );
    for( int i = 0; i < styles.length; i++ )
      removeStyle( styles[i] );
    m_workspace.removeModellListener( this );
  }

  private void setDirty()
  {
    for( Iterator iter = m_styleDisplayMap.values().iterator(); iter.hasNext(); )
    {
      StyleDisplayMap map = (StyleDisplayMap)iter.next();
      map.setDirty();
    }
  }

  public CommandableWorkspace getWorkspace()
  {
    return m_workspace;
  }

  public FeatureType getFeatureType()
  {
    return m_featureType;
  }

  public void paintSelected( final Graphics graphics, final GeoTransform projection,
      final double scale, final GM_Envelope bbox, final int selectionId )
  {
    for( Iterator iter = m_styleDisplayMap.values().iterator(); iter.hasNext(); )
    {
      StyleDisplayMap map = (StyleDisplayMap)iter.next();
      map.paintSelected( graphics, projection, scale, bbox, selectionId );
    }
  }

  public void addStyle( final KalypsoUserStyle style )
  {
    final StyleDisplayMap styleDisplayMap = new StyleDisplayMap( style );
    m_styleDisplayMap.put( style, styleDisplayMap );
    style.addModellListener( this );
  }

  public void removeStyle( final KalypsoUserStyle style )
  {
    style.removeModellListener( this );
    m_styleDisplayMap.remove( style );
  }

  public UserStyle[] getStyles()
  {
    Set set = m_styleDisplayMap.keySet();
    return (UserStyle[])set.toArray( new UserStyle[set.size()] );
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    setDirty();
    fireModellEvent( modellEvent );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox()
  {
    return m_featureList.getBoundingBox();
  }

  public FeatureList getFeatureList()
  {
    return m_featureList;
  }

  /**
   * 
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeatureListVisible(org.deegree.model.geometry.GM_Envelope)
   */
  public FeatureList getFeatureListVisible( GM_Envelope env )
  {
    if( env == null )
      env = getBoundingBox();
    final Set result = new HashSet();
    for( Iterator iter = m_styleDisplayMap.values().iterator(); iter.hasNext(); )
    {
      StyleDisplayMap map = (StyleDisplayMap)iter.next();
      map.queryVisibleFeatures( env, result );
    }
    final FeatureList list = FeatureFactory.createFeatureList();
    list.addAll( result );
    return list;
  }

  /**
   * @see org.kalypso.util.command.ICommandTarget#postCommand(org.kalypso.util.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    try
    {
      m_workspace.postCommand( command );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    runnable.run();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getSchedulingRule()
   */
  public ISchedulingRule getSchedulingRule()
  {
    return null;
  }

  public class StyleDisplayMap
  {
    private final List m_dispayElements = new ArrayList();

    private final UserStyle[] m_style;

    private GM_Envelope m_vaildEnvelope = null;

    private int m_maxDisplayArray = 0;

    public StyleDisplayMap( UserStyle style )
    {
      m_style = new UserStyle[]
      { style };
    }

    public Set queryVisibleFeatures( GM_Envelope env, Set result )
    {
      if( result == null )
        result = new HashSet();
      m_vaildEnvelope=null;
      restyle( env );
      for( Iterator iter = m_dispayElements.iterator(); iter.hasNext(); )
        result.add( ( (DisplayElement[])iter.next() )[0].getFeature() );
      return result;
    }

    public void setDirty()
    {
      m_vaildEnvelope = null;
      m_dispayElements.clear();
    }

    public List getSelectedDisplayElements( List result, int selectionId, GM_Envelope bbox )
    {
      if( result == null )
        result = new ArrayList();

      for( Iterator iter = m_dispayElements.iterator(); iter.hasNext(); )
      {
        DisplayElement[] de = (DisplayElement[])iter.next();
        if( de.length > 0 )
        {
          Feature feature = de[0].getFeature();
          if( feature.isSelected( selectionId ) && feature.getEnvelope().intersects( bbox ) )
            result.add( de );
        }
      }
      return result;
    }

    public void paintSelected( Graphics g, GeoTransform p, double scale, GM_Envelope bbox,
        int selectionId )
    {
      restyle( bbox );
      final List selectedDE = getSelectedDisplayElements( null, selectionId, bbox );
      final List[] layerList = new List[m_maxDisplayArray];
      // try to keep order of rules in userstyle
      for( int i = 0; i < layerList.length; i++ )
        layerList[i] = new ArrayList();
      for( Iterator iter = selectedDE.iterator(); iter.hasNext(); )
      {
        Object object = iter.next();
        DisplayElement[] element = (DisplayElement[])object;
        for( int i = 0; i < element.length; i++ )
        {
          if( element[i].doesScaleConstraintApply( scale ) )
            layerList[i].add( element[i] );
        }
      }
      for( int i = 0; i < layerList.length; i++ )
        for( Iterator iterator = layerList[i].iterator(); iterator.hasNext(); )
  				((DisplayElement)iterator.next()).paint( g, p );
    }

    public void restyle( GM_Envelope env )
    {
      //      m_vaildEnvelope = null;
      if( m_vaildEnvelope == null || !m_vaildEnvelope.contains( env ) )
      { // restyle
        if( m_vaildEnvelope == null )
          m_vaildEnvelope = env;
        else
          m_vaildEnvelope = m_vaildEnvelope.getMerged( env );
        m_dispayElements.clear();
        m_maxDisplayArray = 0;
        List features = m_featureList.query( m_vaildEnvelope, null );
        for( Iterator iter = features.iterator(); iter.hasNext(); )
        {
          Feature feature = (Feature)iter.next();
          try
          {
            DisplayElement[] elements = DisplayElementFactory.createDisplayElement( feature,
                m_style,m_workspace);
            if( elements.length > 0 )
              m_dispayElements.add( elements );
            if( elements.length > m_maxDisplayArray )
              m_maxDisplayArray = elements.length;
          }
          catch( IncompatibleGeometryTypeException e )
          {
            e.printStackTrace();
          }
        }
      }
    }
  }

}