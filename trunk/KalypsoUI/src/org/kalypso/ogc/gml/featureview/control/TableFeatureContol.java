package org.kalypso.ogc.gml.featureview.control;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.kalypso.ogc.gml.KalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ogc.gml.table.celleditors.IFeatureModifierFactory;
import org.kalypso.util.command.DefaultCommandManager;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.IGMLWorkspaceModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;

/**
 * @author belger
 */
public class TableFeatureContol extends AbstractFeatureControl implements ModellEventListener
{
  private final IFeatureModifierFactory m_factory;

  private final int m_selectionID;

  private LayerTableViewer m_viewer;

  private KalypsoFeatureTheme m_kft;

  private final JobExclusiveCommandTarget m_target;

  private Collection m_listeners = new ArrayList();

  public TableFeatureContol( final FeatureTypeProperty ftp, final IFeatureModifierFactory factory,
      final int selectionID )
  {
    super( ftp );

    m_factory = factory;
    m_selectionID = selectionID;
    m_target = new JobExclusiveCommandTarget( new DefaultCommandManager(), null );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite,
   *      int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    m_viewer = new LayerTableViewer( parent, SWT.NONE, m_target, m_factory, m_selectionID, false );

    setFeature( getFeature() );

    return m_viewer.getControl();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#dispose()
   */
  public void dispose()
  {
    if( m_viewer != null )
      m_viewer.dispose();

    if( m_kft != null )
      m_kft.dispose();

    m_target.dispose();

    super.dispose();

  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#setFeature(org.kalypsodeegree.model.feature.Feature)
   */
  public void setFeature( final Feature feature )
  {
    super.setFeature( feature );

    if( m_kft != null )
    {
      m_kft.dispose();
      m_kft = null;
    }

    if( m_viewer != null )
    {
      final String ftpName = getFeatureTypeProperty().getName();

      final GMLWorkspace_Impl workspace = new GMLWorkspace_Impl( null, feature, null, null, null, null );
      final CommandableWorkspace c_workspace = new CommandableWorkspace( workspace );

      m_kft = new KalypsoFeatureTheme( c_workspace, ftpName, ftpName );
      m_kft.addModellListener( this );
      m_viewer.setInput( m_kft );

      // create columns
      // add all columns: TODO: use template?
      final FeatureType featureType = m_kft.getFeatureType();
      final FeatureTypeProperty[] properties = featureType.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        final FeatureTypeProperty ftp = properties[i];
        m_viewer.addColumn( ftp.getName(), true, 100, "SWT.CENTER", null, true );
      }

    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl()
  {
    m_viewer.refresh();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#collectChanges(java.util.Collection)
   */
  public void collectChanges( final Collection c )
  {
  // TODO!
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#isValid()
   */
  public boolean isValid()
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    m_listeners.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
    m_listeners.remove( l );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent != null )
    {
      if( modellEvent instanceof IGMLWorkspaceModellEvent
          && ( (IGMLWorkspaceModellEvent)modellEvent ).getGMLWorkspace() == m_kft.getWorkspace() )
      {
        final FeaturesChangedModellEvent feEvent = (FeaturesChangedModellEvent)modellEvent;
        if( feEvent.getGMLWorkspace() == m_kft.getWorkspace() )
        {
          final Event event = new Event();
          event.widget = m_viewer.getControl();
          final ModifyEvent me = new ModifyEvent( event );
          for( final Iterator mIt = m_listeners.iterator(); mIt.hasNext(); )
            ( (ModifyListener)mIt.next() ).modifyText( me );
        }
      }
    }
  }
}