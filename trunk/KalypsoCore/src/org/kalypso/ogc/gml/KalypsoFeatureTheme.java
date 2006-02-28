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

import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.IGMLWorkspaceModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.sort.SplitSort;

/**
 * @author vdoemming
 */
public class KalypsoFeatureTheme extends AbstractKalypsoTheme implements IKalypsoFeatureTheme
{
  final CommandableWorkspace m_workspace;

  private final HashMap m_styleDisplayMap = new HashMap();

  private final IFeatureType m_featureType;

  protected final FeatureList m_featureList;

  private final IFeatureSelectionManager m_selectionManager;

  public KalypsoFeatureTheme( final CommandableWorkspace workspace, final String featurePath, final String name, final IFeatureSelectionManager selectionManager )
  {
    super( name );

    m_workspace = workspace;
    m_selectionManager = selectionManager;

    final Object featureFromPath = workspace.getFeatureFromPath( featurePath );
    if( featureFromPath instanceof FeatureList )
    {
      m_featureType = workspace.getFeatureTypeFromPath( featurePath );
      m_featureList = (FeatureList) featureFromPath;
    }
    else if( featureFromPath instanceof Feature )
    {
      m_featureList = new SplitSort( null, null );
      m_featureList.add( featureFromPath );
      m_featureType = ((Feature) featureFromPath).getFeatureType();
    }
    else
    {
      m_featureList = new SplitSort( null, null );
      m_featureType = null;
      // throw new IllegalArgumentException( "FeaturePath doesn't point to feature collection: " + featurePath );
    }

    m_workspace.addModellListener( this );
  }

  public void dispose( )
  {
    final Set set = m_styleDisplayMap.keySet();
    final KalypsoUserStyle[] styles = (KalypsoUserStyle[]) set.toArray( new KalypsoUserStyle[set.size()] );
    for( int i = 0; i < styles.length; i++ )
      removeStyle( styles[i] );
    m_workspace.removeModellListener( this );
  }

  private void setDirty( )
  {
    for( final Iterator iter = m_styleDisplayMap.values().iterator(); iter.hasNext(); )
    {
      final StyleDisplayMap map = (StyleDisplayMap) iter.next();
      map.setDirty();
    }
  }

  public CommandableWorkspace getWorkspace( )
  {
    return m_workspace;
  }

  public IFeatureType getFeatureType( )
  {
    return m_featureType;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean)
   */
  public void paint( final Graphics graphics, final GeoTransform projection, final double scale, final GM_Envelope bbox, final boolean selected )
  {
    // find all selected/unselected features in this theme
    final FeatureList featureList = getFeatureList();
    final List globalSelectedFeatures = m_selectionManager.toList();
    final List featuresFilter = new ArrayList( featureList.size() );
    featuresFilter.addAll( featureList );
    if( selected )
      featuresFilter.retainAll( globalSelectedFeatures );
    else
      featuresFilter.removeAll( globalSelectedFeatures );

    for( final Iterator iter = m_styleDisplayMap.values().iterator(); iter.hasNext(); )
    {
      final StyleDisplayMap map = (StyleDisplayMap) iter.next();
      map.paintSelected( graphics, projection, scale, bbox, featuresFilter );
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

  public UserStyle[] getStyles( )
  {
    Set set = m_styleDisplayMap.keySet();
    return (UserStyle[]) set.toArray( new UserStyle[set.size()] );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    if( modellEvent instanceof IGMLWorkspaceModellEvent )
    {
      // my workspace ?
      if( ((IGMLWorkspaceModellEvent) modellEvent).getGMLWorkspace() != m_workspace )
        return; // not my workspace
      if( modellEvent instanceof FeaturesChangedModellEvent )
      {
        final FeaturesChangedModellEvent featuresChangedModellEvent = ((FeaturesChangedModellEvent) modellEvent);
        final List features = featuresChangedModellEvent.getFeatures();
        // optimize: i think it is faster to restyle all than to find and
        // exchange so many display elements
        if( features.size() > m_featureList.size() / 5 )
          setDirty();
        else
        {
          for( Iterator iterator = features.iterator(); iterator.hasNext(); )
          {
            final Feature feature = (Feature) iterator.next();
            // my feature ?
            if( m_featureList.contains( feature ) )
              restyleFeature( feature );
          }
        }

      }
      else if( modellEvent instanceof FeatureStructureChangeModellEvent )
      {
        final FeatureStructureChangeModellEvent fscme = (FeatureStructureChangeModellEvent) modellEvent;
        final Feature[] parents = fscme.getParentFeatures();
        for( int i = 0; i < parents.length; i++ )
        {
          Feature parent = parents[i];
          if( m_featureList.getParentFeature() == parent )
          {
            switch( fscme.getChangeType() )
            {
              case FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD:
              case FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE:
              case FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_MOVE:
                setDirty();
                // restyleFeature( parent);
                break;
              default:
                setDirty();
            }
          }
        }
      }
    }
    else if( modellEvent.isType( ModellEvent.STYLE_CHANGE ) )
      setDirty();
    else
    // unknown event, set dirty
    { // TODO : if the eventhierarchy is implemented correctly the else-part can
      // be removed
      setDirty();
    }
    fireModellEvent( modellEvent );
  }

  private void restyleFeature( Feature feature )
  {
    for( final Iterator iter = m_styleDisplayMap.values().iterator(); iter.hasNext(); )
    {
      StyleDisplayMap sdm = (StyleDisplayMap) iter.next();
      sdm.restyle( feature );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    return m_featureList.getBoundingBox();
  }

  public FeatureList getFeatureList( )
  {
    return m_featureList;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeatureListVisible(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public FeatureList getFeatureListVisible( GM_Envelope env )
  {
    if( env == null )
      env = getBoundingBox();
    final Set result = new HashSet();
    for( final Iterator iter = m_styleDisplayMap.values().iterator(); iter.hasNext(); )
    {
      final StyleDisplayMap map = (StyleDisplayMap) iter.next();
      map.queryVisibleFeatures( env, result );
    }
    final FeatureList list = FeatureFactory.createFeatureList( m_featureList.getParentFeature(), m_featureList.getParentFeatureTypeProperty() );
    list.addAll( result );
    return list;
  }

  /**
   * @see org.kalypso.commons.command.ICommandTarget#postCommand(org.kalypso.commons.command.ICommand,
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
    if( runnable != null )
      runnable.run();
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getSchedulingRule()
   */
  public ISchedulingRule getSchedulingRule( )
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
      m_style = new UserStyle[] { style };
    }

    public Set queryVisibleFeatures( final GM_Envelope env, Set result )
    {
      if( result == null )
        result = new HashSet();
      m_vaildEnvelope = null;
      restyle( env );

      // iterate through array to avoid concurrent modification exception
      final DisplayElement[][] elts = (DisplayElement[][]) m_dispayElements.toArray( new DisplayElement[m_dispayElements.size()][] );
      for( int i = 0; i < elts.length; i++ )
      {
        final DisplayElement[] element = elts[i];
        result.add( element[0].getFeature() );
      }

      return result;
    }

    public void setDirty( )
    {
      m_vaildEnvelope = null;
      m_dispayElements.clear();
      // TODO ??
      m_maxDisplayArray = 0;
    }

    /**
     * @return List of display elements that fit to selectionMask <br>
     */
    public List getSelectedDisplayElements( final List featuresToFilter, final GM_Envelope bbox )
    {
      final List result = new ArrayList();
      for( int i = 0; i < m_dispayElements.size(); i++ )
      {
        final DisplayElement[] de = (DisplayElement[]) m_dispayElements.get( i );
        if( de.length > 0 )
        {
          final Feature feature = de[0].getFeature();

          if( featuresToFilter.contains( feature ) && feature.getEnvelope().intersects( bbox ) )
            result.add( de );
        }
      }
      return result;
    }

    void paintSelected( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox, final List featureFilter )
    {
      restyle( bbox );

      final List selectedDE = getSelectedDisplayElements( featureFilter, bbox );

      final List[] layerList = new List[m_maxDisplayArray];

      // try to keep order of rules in userstyle
      for( int i = 0; i < layerList.length; i++ )
        layerList[i] = new ArrayList();

      for( final Iterator iter = selectedDE.iterator(); iter.hasNext(); )
      {
        final DisplayElement[] element = (DisplayElement[]) iter.next();
        for( int i = 0; i < element.length; i++ )
        {
          if( element[i].doesScaleConstraintApply( scale ) )
            layerList[i].add( element[i] );
        }
      }

      for( int i = 0; i < layerList.length; i++ )
      {
        for( final Iterator iterator = layerList[i].iterator(); iterator.hasNext(); )
          ((DisplayElement) iterator.next()).paint( g, p );
      }
    }

    public void restyle( final GM_Envelope env )
    {
      if( env != null && (m_vaildEnvelope == null || !m_vaildEnvelope.contains( env )) )
      { // restyle
        if( m_vaildEnvelope == null )
          m_vaildEnvelope = env;
        else
          m_vaildEnvelope = m_vaildEnvelope.getMerged( env );
        m_dispayElements.clear();
        m_maxDisplayArray = 0;
        final List features = m_featureList.query( m_vaildEnvelope, null );
        for( Iterator iter = features.iterator(); iter.hasNext(); )
        {

          final Object next = iter.next();
          if( next instanceof Feature )
            addDisplayElements( (Feature) next );
          else
            System.out.println( "dsssebug" );
        }
      }
    }

    private void addDisplayElements( Feature feature )
    {
      DisplayElement[] elements = DisplayElementFactory.createDisplayElement( feature, m_style, m_workspace );
      if( elements.length > 0 )
        m_dispayElements.add( elements );
      if( elements.length > m_maxDisplayArray )
        m_maxDisplayArray = elements.length;
    }

    public void restyle( Feature feature )
    {
      for( Iterator iter = m_dispayElements.iterator(); iter.hasNext(); )
      {
        DisplayElement[] elements = (DisplayElement[]) iter.next();
        if( elements[0].getFeature().equals( feature ) )
        {
          m_dispayElements.remove( elements );
          break;
        }
      }
      addDisplayElements( feature );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getSelectionManager()
   */
  public IFeatureSelectionManager getSelectionManager( )
  {
    return m_selectionManager;
  }
}