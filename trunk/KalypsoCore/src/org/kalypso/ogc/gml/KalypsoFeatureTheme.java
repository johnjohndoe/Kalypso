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
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.KalypsoCorePlugin;
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
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.sort.SplitSort;

/**
 * @author Andreas von D�mming
 */
public class KalypsoFeatureTheme extends AbstractKalypsoTheme implements IKalypsoFeatureTheme, ModellEventListener, IKalypsoUserStyleListener
{
  /* Preserve order of styles. */
  private final Map<KalypsoUserStyle, UserStylePainter> m_styleDisplayMap = new LinkedHashMap<KalypsoUserStyle, UserStylePainter>();

  private final CommandableWorkspace m_workspace;

  private final IFeatureType m_featureType;

  private final FeatureList m_featureList;

  private final IFeatureSelectionManager m_selectionManager;

  private final String m_featurePath;

  static interface IPaintDelegate
  {
    public void paint( final DisplayElement displayElement );
  }

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

    m_workspace.addModellListener( this );
  }

  @Override
  public void dispose( )
  {
    final Set<KalypsoUserStyle> set = m_styleDisplayMap.keySet();
    final KalypsoUserStyle[] styles = set.toArray( new KalypsoUserStyle[set.size()] );
    for( final KalypsoUserStyle element : styles )
      removeStyle( element );

    m_workspace.removeModellListener( this );

    super.dispose();
  }

  private void setDirty( )
  {
    invalidate( getBoundingBox() );
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
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean,
   *      org.kalypso.contribs.eclipse.core.runtime.IProgressMonitorWithFactory)
   */
  public void paint( final Graphics graphics, final GeoTransform projection, final double scale, final GM_Envelope bbox, final boolean selected, final IProgressMonitor monitor ) throws CoreException
  {
    final Collection<UserStylePainter> styles = m_styleDisplayMap.values();
    final UserStylePainter[] styleArray = styles.toArray( new UserStylePainter[styles.size()] );

    final SubMonitor progress = SubMonitor.convert( monitor, "Zeichne Styles", styleArray.length );

    if( m_featureList == null )
      return;

    final GMLWorkspace workspace = getWorkspace();

    for( final UserStylePainter map : styleArray )
    {
      final SubMonitor childProgress = progress.newChild( 1 );
      map.paintSelected( workspace, graphics, projection, scale, bbox, m_featureList, selected, childProgress );
      ProgressUtilities.done( childProgress );
    }
  }

  public void addStyle( final KalypsoUserStyle style )
  {
    final UserStylePainter styleDisplayMap = new UserStylePainter( style, m_selectionManager );
    m_styleDisplayMap.put( style, styleDisplayMap );
    style.addStyleListener( this );
  }

  public void removeStyle( final KalypsoUserStyle style )
  {
    style.removeStyleListener( this );
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
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( m_featureList == null )
      return;

    if( modellEvent instanceof IGMLWorkspaceModellEvent )
    {
      // my workspace ?
      final GMLWorkspace changedWorkspace = ((IGMLWorkspaceModellEvent) modellEvent).getGMLWorkspace();
      if( (changedWorkspace != m_workspace && changedWorkspace != m_workspace.getWorkspace()) )
        return; // not my workspace

      if( modellEvent instanceof FeaturesChangedModellEvent )
      {
        final FeaturesChangedModellEvent featuresChangedModellEvent = ((FeaturesChangedModellEvent) modellEvent);
        final Feature[] features = featuresChangedModellEvent.getFeatures();

        // TODO: BOTH ways (if and else) are mayor performance bugs.
        // we MUST first determine if we have to restyle at all that is, if this modell event
        // did change any features belonging to me

        // optimize: i think it is faster to restyle all than to find and
        // exchange so many display elements
        if( features.length > m_featureList.size() / 5 )
          setDirty();
        else
        {
          for( final Feature feature : features )
            restyleFeature( feature );
        }
      }
      else if( modellEvent instanceof FeatureStructureChangeModellEvent )
      {
        final FeatureStructureChangeModellEvent fscme = (FeatureStructureChangeModellEvent) modellEvent;
        final Feature[] parents = fscme.getParentFeatures();
        for( final Feature parent : parents )
        {
          if( m_featureList.getParentFeature() == parent )
          {
            switch( fscme.getChangeType() )
            {
              case FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD:
                // fall through
              case FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE:
                // fall through
              case FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_MOVE:
                setDirty();
                break;
              default:
                setDirty();
            }
          }
        }
      }
    }
    else
      // unknown event, set dirty
      // TODO : if the eventhierarchy is implemented correctly the else-part can be removed
      setDirty();
  }

  private void restyleFeature( final Feature feature )
  {
    // my feature ?
    // FIXME: SLOW!! This is a major performance bug
    if( !m_featureList.contains( feature ) )
      return;

    // TODO: invalidation should made via the screen-rectangle of this feature
    // depending on the syled geometry
    invalidate( feature.getEnvelope() );
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
  public FeatureList getFeatureListVisible( final GM_Envelope searchEnvelope )
  {
    if( m_featureList == null )
      return null;

    /* Use complete bounding box if search envelope is not set. */
    final GM_Envelope env = searchEnvelope == null ? getBoundingBox() : searchEnvelope;

    final FeatureList resultList = FeatureFactory.createFeatureList( m_featureList.getParentFeature(), m_featureList.getParentFeatureTypeProperty() );
    final GMLWorkspace workspace = getWorkspace();

    final IPaintDelegate paintDelegate = new IPaintDelegate()
    {
      public void paint( final DisplayElement displayElement )
      {
        final Feature feature = displayElement.getFeature();
        GM_Envelope envelope = feature.getEnvelope();
        if( envelope != null && env.intersects( envelope ) )
          resultList.add( feature );
      }
    };

    final IProgressMonitor monitor = new NullProgressMonitor();

    for( final UserStylePainter map : m_styleDisplayMap.values() )
    {
      final Boolean selected = null; // selection is not considered here
      final Double scale = null; // TODO: scale is not considered here, but it should
      try
      {
        map.paintFeatureTypeStyles( workspace, scale, env, m_featureList, selected, monitor, paintDelegate );
      }
      catch( final CoreException e )
      {
        KalypsoCorePlugin.getDefault().getLog().log( e.getStatus() );
      }
    }

    return resultList;
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
      // TODO: error handling
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
    if( (workspace != null) && workspace.isDirty() )
      return label + "*";

    return label;
  }

  /**
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getImageDescriptor(java.lang.Object)
   */
  @Override
  public ImageDescriptor getImageDescriptor( final Object object )
  {
    final Object[] children = getChildren( this );
    if( (children.length > 0) && (children[0] instanceof IWorkbenchAdapter) )
      return ((IWorkbenchAdapter) children[0]).getImageDescriptor( children[0] );

    return super.getImageDescriptor( object );
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

  /**
   * @see org.kalypso.ogc.gml.IKalypsoUserStyleListener#styleChanged(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void styleChanged( final KalypsoUserStyle source )
  {
    setDirty();
  }
}