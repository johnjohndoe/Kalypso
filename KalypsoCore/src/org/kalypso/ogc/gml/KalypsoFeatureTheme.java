package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureList;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.sort.DisplayContext;
import org.deegree_impl.model.sort.SplitSort;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.util.command.ICommand;

/**
 * @author vdoemming
 */
public class KalypsoFeatureTheme extends AbstractKalypsoTheme implements IKalypsoFeatureTheme
{
  public final static UserStyle[] NO_STYLE = new UserStyle[0];

  private CommandableWorkspace m_workspace;

  private boolean m_isdirty = true;

  private List m_styles = new ArrayList();

  private Map m_displayContexts = new HashMap();

  private final FeatureList m_featureList;

  private final FeatureType m_featureType;

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
    m_workspace.removeModellListener( this );
  }

  private void setDirty( final boolean dirty )
  {
    m_isdirty = dirty;
  }

  public CommandableWorkspace getWorkspace()
  {
    return m_workspace;
  }

  public FeatureType getFeatureType()
  {
    return m_featureType;
  }

  public void paintSelected( final Graphics g, final GeoTransform p, final double scale,
      final GM_Envelope bbox, final int selectionId )
  {
    if( m_isdirty )
      reStyleAll();

    final Collection displaycontexts = m_displayContexts.values();

    for( int i = 0; i < m_styles.size(); i++ )
    {
      for( final Iterator it = displaycontexts.iterator(); it.hasNext(); )
      {
        final DisplayContext context = (DisplayContext)it.next();
        if( selectionId == -1 || context.getFeature().isSelected( selectionId ) )
          context.paint( g, p, i );
      }
    }
  }

  public void addStyle( final KalypsoUserStyle style )
  {
    m_styles.add( style );

    style.addModellListener( this );

    setDirty( true );
  }

  public void removeStyle( final KalypsoUserStyle style )
  {
    style.removeModellListener( this );

    m_styles.remove( style );

    setDirty( true );
  }

  private void reStyleAll()
  {
    // alle display elemente neu generieren
    final UserStyle[] styles = getStyles();

    for( final Iterator iter = m_featureList.iterator(); iter.hasNext(); )
    {
      final Feature feature = (Feature)iter.next();
      m_displayContexts.put( feature, new DisplayContext( feature, styles ) );
    }

    setDirty( false );
  }

  public UserStyle[] getStyles()
  {
    return (UserStyle[])m_styles.toArray( new UserStyle[m_styles.size()] );
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    setDirty( true );

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

}