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
package org.kalypso.ui.editor.featureeditor;

import java.io.Reader;
import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.FeaturetemplateType.LayerType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.xml.sax.InputSource;

/**
 * @author belger
 */
public class FeatureTemplateviewer implements IPoolListener, ModellEventListener
{
  private final ObjectFactory m_templateFactory = new ObjectFactory();

  protected final Marshaller m_marshaller;

  private final Unmarshaller m_unmarshaller;

  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool();

  protected Composite m_panel;

  private IPoolableObjectType m_key;

  private CommandableWorkspace m_workspace;

  private String m_featurePath;

  private FeatureComposite m_featureComposite = new FeatureComposite( null, new URL[] {} );

  private Label m_label;

  private final IFeatureChangeListener m_changeListener = new IFeatureChangeListener()
  {
    public void featureChanged( final FeatureChange change )
    {
      onFeatureChanged( change );
    }
  };

  private final JobExclusiveCommandTarget m_commandtarget;

  public FeatureTemplateviewer( final JobExclusiveCommandTarget commandtarget )
  {
    m_commandtarget = commandtarget;

    try
    {
      m_marshaller = m_templateFactory.createMarshaller();
      m_marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

      m_unmarshaller = m_templateFactory.createUnmarshaller();
    }
    catch( final JAXBException e )
    {
      // sollte nie passieren
      e.printStackTrace();

      throw new RuntimeException( e );
    }
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose()
  {
    setWorkspace( null );

    m_featureComposite.dispose();

    m_pool.removePoolListener( this );
  }

  public IStatus saveGML( final IProgressMonitor monitor )
  {
    try
    {
      m_pool.saveObject( m_workspace, monitor );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      return KalypsoGisPlugin.createErrorStatus( "Fehler beim Speichern", e );
    }

    return Status.OK_STATUS;
  }

  public final void loadInput( final Reader reader, final URL context,
      final IProgressMonitor monitor, Properties props ) throws CoreException
  {
    monitor.beginTask( "Ansicht laden", 1000 );
    try
    {
      final InputSource is = new InputSource( reader );

      final Featuretemplate m_template = (Featuretemplate)m_unmarshaller.unmarshal( is );

      final List views = m_template.getView();
      for( Iterator iter = views.iterator(); iter.hasNext(); )
        m_featureComposite.addView( (FeatureviewType)iter.next() );

      final LayerType layer = m_template.getLayer();
      final String href;
      final String linktype;
      if( layer != null )
      {
        m_featurePath = layer.getFeaturePath();
        href = layer.getHref();
        linktype = layer.getLinktype();
      }
      else
      {
        m_featurePath = props.getProperty( "featurepath", "/" );
        href = props.getProperty( "href", "" );
        linktype = props.getProperty( "linktype", "gml" );
      }
      m_key = new PoolableObjectType( linktype, href, context );
      m_pool.addPoolListener( this, m_key );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();

      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Fehler beim Lesen der Vorlage",
          e ) );
    }
    finally
    {
      monitor.done();
    }
  }

  private void setWorkspace( final CommandableWorkspace workspace )
  {
    if( m_workspace != null )
      m_workspace.removeModellListener( this );

    m_workspace = workspace;

    if( m_workspace != null )
      m_workspace.addModellListener( this );

    m_commandtarget.setCommandManager( workspace );

    if( m_panel == null || m_panel.isDisposed() )
      return;

    m_panel.getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        updateControls();
      }
    } );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key, final Object newValue,
      final IStatus status )
  {
    // Daten sind jetzt da!
    if( m_pool.equalsKeys( m_key, key ) )
      setWorkspace( (CommandableWorkspace)newValue );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType,
   *      java.lang.Object)
   */
  public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
  {
    if( m_pool.equalsKeys( key, m_key ) )
      setWorkspace( null );
  }

  public void createControls( final Composite parent, final int style )
  {
    final ScrolledComposite scrolledComposite = new ScrolledComposite( parent, SWT.H_SCROLL
        | SWT.V_SCROLL | style );
    //    scrolledComposite.setLayout( new GridLayout( ) );
    //    final Composite composite = m_templateviewer.createControls(
    // scrolledComposite );
    //    composite.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    m_panel = new Composite( scrolledComposite, SWT.NONE );
    m_panel.setLayout( new GridLayout() );

    updateControls();
    scrolledComposite.setContent( m_panel );
  }

  protected void updateControls()
  {
    try
    {
      if( m_label != null && !m_label.isDisposed() )
        m_label.dispose();

      m_featureComposite.setFeature( null );
      m_featureComposite.disposeControl();

      m_featureComposite.setFeature( null );
      m_featureComposite.updateControl();

      if( m_workspace == null )
      {
        if( m_panel != null )
        {
          m_label = new Label( m_panel, SWT.CENTER );
          m_label.setText( "laden..." );
          m_label.setLayoutData( new GridData( GridData.FILL_BOTH ) );
        }

        return;
      }

      final Object featureFromPath = m_workspace.getFeatureFromPath( m_featurePath );
      if( featureFromPath instanceof Feature )
      {
        final Feature feature = (Feature)featureFromPath;

        m_featureComposite.setFeature( feature );
        m_featureComposite.createControl( m_panel, SWT.NONE, feature.getFeatureType() );
        m_featureComposite.setFeature( feature );
        m_featureComposite.updateControl();

        m_panel.layout();

        m_featureComposite.addChangeListener( m_changeListener );

        m_commandtarget.resetDirty();
      }
      else
      {
        // todo Fehlermeldung anzeigen
      }
    }
    finally
    {
      m_panel.setSize( m_panel.computeSize( SWT.DEFAULT, SWT.DEFAULT ) );
    }
  }

  protected void onFeatureChanged( final FeatureChange change )
  {
    m_commandtarget.postCommand( new ChangeFeaturesCommand( m_workspace, new FeatureChange[]
    { change } ), null );
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    final FeatureComposite featureComposite = m_featureComposite;
    if( m_panel != null )
    {
      m_panel.getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          featureComposite.updateControl();
        }
      } );
    }
  }
  
  public Feature getFeature()
  {
    return m_featureComposite.getFeature();
  }
}