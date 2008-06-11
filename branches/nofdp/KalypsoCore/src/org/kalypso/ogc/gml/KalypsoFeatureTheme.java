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
import java.net.URL;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.KalypsoCoreDebug;
import org.kalypso.core.KalypsoCoreExtensions;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.i18n.Messages;
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
 * @author Andreas von Dömming
 */
public class KalypsoFeatureTheme extends AbstractKalypsoTheme implements IKalypsoFeatureTheme, ModellEventListener, IKalypsoUserStyleListener
{
  /* Preserve order of styles. */
  private final Map<KalypsoUserStyle, UserStylePainter> m_styleDisplayMap = new LinkedHashMap<KalypsoUserStyle, UserStylePainter>();

  private CommandableWorkspace m_workspace;

  private final IFeatureType m_featureType;

  private final FeatureList m_featureList;

  private final IFeatureSelectionManager m_selectionManager;

  private final String m_featurePath;

  /**
   * Holds the descriptor for the default icon of this theme. Is used in legends, such as the outline.
   */
  private Image m_featureThemeIcon;

  public KalypsoFeatureTheme( final CommandableWorkspace workspace, final String featurePath, final I10nString name, final IFeatureSelectionManager selectionManager, final IMapModell mapModel, final String legendIcon, final URL context, final boolean shouldShowChildren )
  {
    super( name, "FeatureTheme", mapModel, legendIcon, context, shouldShowChildren ); //$NON-NLS-1$

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

    if( m_workspace != null )
    {
      m_workspace.removeModellListener( this );
      m_workspace = null;
    }

    if( m_featureThemeIcon != null )
      m_featureThemeIcon.dispose();

    super.dispose();
  }

  private void setDirty( )
  {
    invalidate( getFullExtent() );
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
   * @see org.kalypso.ogc.gml.IKalypsoFeatureTheme#getFeaturePath()
   */
  public String getFeaturePath( )
  {
    return m_featurePath;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, boolean,
   *      org.kalypso.contribs.eclipse.core.runtime.IProgressMonitorWithFactory)
   */
  public void paint( final Graphics graphics, final GeoTransform projection, final double scale, final GM_Envelope bbox, final boolean selected, final IProgressMonitor monitor ) throws CoreException
  {
    final IPaintDelegate paintDelegate = new IPaintDelegate()
    {
      public void paint( final DisplayElement displayElement, final IProgressMonitor paintMonitor ) throws CoreException
      {
        displayElement.paint( graphics, projection, paintMonitor );

        // DEBUG output to show feature envelope
        // final GM_Envelope envelope = displayElement.getFeature().getEnvelope();
        // if( envelope != null )
        // {
        // GM_Position destPointMin = projection.getDestPoint( envelope.getMin() );
        // GM_Position destPointMax = projection.getDestPoint( envelope.getMax() );
        //
        // GM_Envelope_Impl env = new GM_Envelope_Impl( destPointMin, destPointMax, null );
        //
        // graphics.drawRect( (int) env.getMin().getX(), (int) env.getMin().getY(), (int) env.getWidth(), (int)
        // env.getHeight()
        // );
        // }
      }
    };

    paint( scale, bbox, selected, monitor, paintDelegate );

    if( m_featureList != null && KalypsoCoreDebug.SPATIAL_INDEX_PAINT.isEnabled() )
      m_featureList.paint( graphics, projection );
  }

  private void paint( final double scale, final GM_Envelope bbox, final boolean selected, final IProgressMonitor monitor, final IPaintDelegate delegate ) throws CoreException
  {
    final Collection<UserStylePainter> styles = m_styleDisplayMap.values();
    final UserStylePainter[] styleArray = styles.toArray( new UserStylePainter[styles.size()] );

    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString("org.kalypso.ogc.gml.KalypsoFeatureTheme.1"), styleArray.length ); //$NON-NLS-1$

    if( m_featureList == null )
      return;

    final GMLWorkspace workspace = getWorkspace();

    for( final UserStylePainter map : styleArray )
    {
      final SubMonitor childProgress = progress.newChild( 1 );
      map.paintSelected( workspace, scale, bbox, m_featureList, selected, childProgress, delegate );
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
      if( ((m_workspace != null) && (changedWorkspace != m_workspace) && (changedWorkspace != m_workspace.getWorkspace())) )
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
          for( final Feature feature : features )
            restyleFeature( feature );
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
  public GM_Envelope getFullExtent( )
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
    final GM_Envelope env = searchEnvelope == null ? getFullExtent() : searchEnvelope;

    final FeatureList resultList = FeatureFactory.createFeatureList( m_featureList.getParentFeature(), m_featureList.getParentFeatureTypeProperty() );
    final GMLWorkspace workspace = getWorkspace();

    final IPaintDelegate paintDelegate = new IPaintDelegate()
    {
      public void paint( final DisplayElement displayElement, final IProgressMonitor paintMonitor )
      {
        final Feature feature = displayElement.getFeature();
        GM_Envelope envelope = feature.getEnvelope();
        if( (envelope != null) && env.intersects( envelope ) )
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
   * @see org.kalypso.ogc.gml.AbstractKalypsoTheme#getDefaultIcon()
   */
  @Override
  protected ImageDescriptor getDefaultIcon( )
  {
    if( m_featureThemeIcon == null )
      m_featureThemeIcon = new Image( Display.getCurrent(), getClass().getResourceAsStream( "resources/featureTheme.gif" ) ); //$NON-NLS-1$

    return ImageDescriptor.createFromImage( m_featureThemeIcon );
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
    fireStatusChanged();
  }

  public void paintInternal( final IPaintInternalDelegate delegate ) throws CoreException
  {
    paint( delegate.getScale(), delegate.getBoundingBox(), delegate.getSelected(), new NullProgressMonitor(), delegate );
  }

  /**
   * @see org.eclipse.core.runtime.PlatformObject#getAdapter(java.lang.Class)
   */
  @SuppressWarnings("unchecked") //$NON-NLS-1$
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( adapter == IKalypsoThemeInfo.class )
      return createThemeInfo();

    return super.getAdapter( adapter );
  }

  private IKalypsoThemeInfo createThemeInfo( )
  {
    final IFeatureType featureType = getFeatureType();
    if( featureType == null )
      return null; // no data available; maybe show some status-message?

    /* If an explicit info is configured for this map, use it */
    // REMARK: is necessary to copy this from AbstractFeatureTheme, as this adapter must be called first
    final String themeInfoId = getProperty( IKalypsoTheme.PROPERTY_THEME_INFO_ID, null );
    if( themeInfoId != null )
      return KalypsoCoreExtensions.createThemeInfo( themeInfoId, this );

    // HACK: use featureThemeInfo from KalypsoUI as a default. This is needed, because this feature info the
    // featureType-properties mechanisms from KalypsoUI in order find a registered featureThemeInfo for the current
    // qname
    final IKalypsoThemeInfo defaultFeatureThemeInfo = KalypsoCoreExtensions.createThemeInfo( "org.kalypso.ui.featureThemeInfo.default", this ); //$NON-NLS-1$
    if( defaultFeatureThemeInfo != null )
      return defaultFeatureThemeInfo;

    return new FeatureThemeInfo( this, new Properties() );
  }
}