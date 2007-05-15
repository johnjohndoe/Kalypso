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
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.IGMLWorkspaceModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.sort.SplitSort;

/**
 * @author Andreas von Dömming
 */
public class KalypsoFeatureTheme extends AbstractKalypsoTheme implements IKalypsoFeatureTheme
{
  final CommandableWorkspace m_workspace;

  private final HashMap<KalypsoUserStyle, StyleDisplayMap> m_styleDisplayMap = new HashMap<KalypsoUserStyle, StyleDisplayMap>();

  private final IFeatureType m_featureType;

  private final FeatureList m_featureList;

  private final IFeatureSelectionManager m_selectionManager;

  private final String m_featurePath;

  public KalypsoFeatureTheme( final CommandableWorkspace workspace, final String featurePath, final String name, final IFeatureSelectionManager selectionManager, final IMapModell mapModel )
  {
    super( name, "FeatureTheme", mapModel );

    m_workspace = workspace;
    m_featurePath = featurePath;
    m_selectionManager = selectionManager;

    final Object featureFromPath = m_workspace.getFeatureFromPath( m_featurePath );
    if( featureFromPath instanceof FeatureList )
    {
      m_featureList = (FeatureList) featureFromPath;
      m_featureType = m_workspace.getFeatureTypeFromPath( m_featurePath );
    }
    else if( featureFromPath instanceof Feature )
    {
      final Feature singleFeature = (Feature) featureFromPath;
      final Feature parent = singleFeature.getParent();
      m_featureList = new SplitSort( parent, singleFeature.getParentRelation() );
      m_featureList.add( singleFeature );
      m_featureType = singleFeature.getFeatureType();
    }
    else
    {
      m_featureList = null;
      m_featureType = null;
    }

    if( m_featureList != null && m_featureList.getParentFeature() == null )
    {
      String message =
        String.format( 
            "BAD: layer with no feature list or feature listList.getParent is empty: "+
            "\n\tfeaturePath = %s\n\tfeatureFromPath = %s", 
            featurePath,
            featureFromPath );
      System.out.println( message );
    }

    m_workspace.addModellListener( this );
  }

  @Override
  public void dispose( )
  {
    final Set<KalypsoUserStyle> set = m_styleDisplayMap.keySet();
    final KalypsoUserStyle[] styles = set.toArray( new KalypsoUserStyle[set.size()] );
    for( int i = 0; i < styles.length; i++ )
      removeStyle( styles[i] );
    m_workspace.removeModellListener( this );

    super.dispose();
  }

  private void setDirty( )
  {
    for( final StyleDisplayMap styleDisplayMap : m_styleDisplayMap.values() )
      styleDisplayMap.setDirty();
  }

  public CommandableWorkspace getWorkspace( )
  {
    return m_workspace;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeatureType()
   */
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
    if( featureList == null )
      return;

    final List<Feature> globalSelectedFeatures = m_selectionManager.toList();
    final List featuresFilter = new ArrayList( featureList.size() );

    final Feature parentFeature = featureList.getParentFeature();
    final GMLWorkspace workspace;
    if( parentFeature == null )
    {
      // this handles the special case where we are just showing the root feature
      if( featureList.size() > 0 )
      {
        final Object object = featureList.get( 0 );
        workspace = object instanceof Feature ? ((Feature) object).getWorkspace() : null;
      }
      else
        workspace = null;
    }
    else
      workspace = parentFeature.getWorkspace();

    for( final Object o : featureList )
    {
      final Feature feature;
      if( workspace == null )
        feature = o instanceof Feature ? (Feature) o : null;
      else
        feature = FeatureHelper.getFeature( workspace, o );

      if( feature != null )
        featuresFilter.add( feature );
    }

    if( selected )
      featuresFilter.retainAll( globalSelectedFeatures );
    else
      featuresFilter.removeAll( globalSelectedFeatures );

    for( final StyleDisplayMap map : m_styleDisplayMap.values() )
      map.paintSelected( graphics, projection, scale, bbox, featuresFilter );
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
    final Set<KalypsoUserStyle> set = m_styleDisplayMap.keySet();
    return set.toArray( new UserStyle[set.size()] );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  @Override
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( m_featureList == null )
      return;

    if( modellEvent instanceof IGMLWorkspaceModellEvent )
    {
      // my workspace ?
      final GMLWorkspace changedWorkspace = ((IGMLWorkspaceModellEvent) modellEvent).getGMLWorkspace();
      if( m_workspace == null || (changedWorkspace != m_workspace && changedWorkspace != m_workspace.getWorkspace()) )
        return; // not my workspace

      if( modellEvent instanceof FeaturesChangedModellEvent )
      {
        final FeaturesChangedModellEvent featuresChangedModellEvent = ((FeaturesChangedModellEvent) modellEvent);
        final Feature[] features = featuresChangedModellEvent.getFeatures();

        // TODO: BOTH ways (if and else) are mayor performance bugs.
        // we MUST first determine if we zhave to restyle at all that is, if this modell event
        // did change any features belonging to me

        // optimize: i think it is faster to restyle all than to find and
        // exchange so many display elements
        if( features.length > m_featureList.size() / 5 )
          setDirty();
        else
        {
          for( final Feature feature : features )
          {
            // my feature ?
            // TODO: SLOW!!
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
    {
      // unknown event, set dirty
      // TODO : if the eventhierarchy is implemented correctly the else-part can be removed
      setDirty();
    }
    fireModellEvent( modellEvent );
  }

  private void restyleFeature( final Feature feature )
  {
    for( final StyleDisplayMap styleDisplayMap : m_styleDisplayMap.values() )
      styleDisplayMap.restyle( feature );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    return m_featureList == null ? null : m_featureList.getBoundingBox();
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
    if( m_featureList == null )
      return null;

    if( env == null )
      env = getBoundingBox();
    final Set<Feature> result = new HashSet<Feature>();
    for( final StyleDisplayMap map : m_styleDisplayMap.values() )
      map.queryVisibleFeatures( env, result );
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
    private final Map<Feature, DisplayElement[]> m_displayElements = new HashMap<Feature, DisplayElement[]>();

    private final UserStyle[] m_style;

    private GM_Envelope m_vaildEnvelope = null;

    private int m_maxDisplayArray = 0;

    public StyleDisplayMap( final UserStyle style )
    {
      m_style = new UserStyle[] { style };
    }

    public Set<Feature> queryVisibleFeatures( final GM_Envelope env, Set<Feature> result )
    {
      if( result == null )
        result = new HashSet<Feature>();
      m_vaildEnvelope = null;
      restyle( env );

      // iterate through array to avoid concurrent modification exception
      final Set<Feature> keySet = m_displayElements.keySet();
      final Feature[] features = keySet.toArray( new Feature[keySet.size()] );
      for( final Feature f : features )
        result.add( f );
      return result;
    }

    public void setDirty( )
    {
      m_vaildEnvelope = null;
      m_displayElements.clear();
      // TODO ??
      m_maxDisplayArray = 0;
    }

    /**
     * @return List of display elements that fit to selectionMask <br>
     */
    public List<DisplayElement[]> getSelectedDisplayElements( final List featuresToFilter, final GM_Envelope bbox )
    {
      final List<DisplayElement[]> result = new ArrayList<DisplayElement[]>();

      for( final Object f : featuresToFilter )
      {
        final Feature feature = (Feature) f;
        final GM_Envelope envelope = feature.getEnvelope();
        if( envelope != null && envelope.intersects( bbox ) )
        {
          final DisplayElement[] elements = m_displayElements.get( feature );
          if( elements != null )
            result.add( elements );
        }
      }

      return result;
    }

    public void paintSelected( final Graphics g, final GeoTransform p, final double scale, final GM_Envelope bbox, final List featureFilter )
    {
      restyle( bbox );

      final List<DisplayElement[]> selectedDE = getSelectedDisplayElements( featureFilter, bbox );
      // final List<DisplayElement[]> selectedDE = m_displayElements;

      final List<DisplayElement>[] layerList = new List[m_maxDisplayArray];

      // try to keep order of rules in userstyle
      for( int i = 0; i < layerList.length; i++ )
        layerList[i] = new ArrayList<DisplayElement>();

      for( final DisplayElement[] element : selectedDE )
      {
        for( int i = 0; i < element.length; i++ )
        {
          if( element[i].doesScaleConstraintApply( scale ) )
            layerList[i].add( element[i] );
        }
      }

      for( int i = 0; i < layerList.length; i++ )
      {
        for( final DisplayElement de : layerList[i] )
          de.paint( g, p );
      }
    }

    public void restyle( final GM_Envelope env )
    {
      if( m_featureList != null && env != null && (m_vaildEnvelope == null || !m_vaildEnvelope.contains( env )) )
      { // restyle
        if( m_vaildEnvelope == null )
          m_vaildEnvelope = env;
        else
          m_vaildEnvelope = m_vaildEnvelope.getMerged( env );
        m_displayElements.clear();
        m_maxDisplayArray = 0;
        final List features = m_featureList.query( m_vaildEnvelope, null );
        for( final Object next : features )
        {
          if( next instanceof Feature )
            addDisplayElements( (Feature) next );

          if( next instanceof String )
          {
            GMLWorkspace workspace = m_featureList.getParentFeature().getWorkspace();
            Feature feature = FeatureHelper.getFeature( workspace, next );
            addDisplayElements( feature );
          }
        }
      }
    }

    private void addDisplayElements( Feature feature )
    {
      final DisplayElement[] elements = DisplayElementFactory.createDisplayElement( feature, m_style, m_workspace );
      if( elements.length > 0 )
        m_displayElements.put( feature, elements );
      if( elements.length > m_maxDisplayArray )
        m_maxDisplayArray = elements.length;
    }

    public void restyle( final Feature feature )
    {
      m_displayElements.remove( feature );
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
  
  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getLabel(java.lang.Object)
   */
  @Override
  public String getLabel( final Object o )
  {
    final String label = super.getLabel( o );

    final CommandableWorkspace workspace = getWorkspace();
    // TODO: change this later to a label decorator?
    if( workspace != null && workspace.isDirty() )
      return label + "*";
    
    return label;
  }
  
  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object o )
  {
    if( o != this )
      throw new IllegalStateException();
    
    final UserStyle[] styles = getStyles();
    if( styles != null )
    {
      final ThemeStyleTreeObject[] result = new ThemeStyleTreeObject[styles.length];
      for( int i = 0; i < styles.length; i++ )
        result[i] = new ThemeStyleTreeObject( this, styles[i] );
      return result;
    }

    return super.getChildren( o );
  }
}