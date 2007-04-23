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
import java.util.List;
import java.util.Properties;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.swt.custom.ScrolledCompositeCreator;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.control.FeatureComposite;
import org.kalypso.ogc.gml.featureview.maker.CachedFeatureviewFactory;
import org.kalypso.ogc.gml.featureview.maker.FeatureviewHelper;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionManager2;
import org.kalypso.template.featureview.Featuretemplate;
import org.kalypso.template.featureview.FeatureviewType;
import org.kalypso.template.featureview.ObjectFactory;
import org.kalypso.template.featureview.Featuretemplate.Layer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.KeyComparator;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.xml.sax.InputSource;

/**
 * @author belger
 */
public class FeatureTemplateviewer implements IPoolListener, ModellEventListener
{
  private final static JAXBContext JC = JaxbUtilities.createQuiet( ObjectFactory.class );

  private final ResourcePool m_pool = KalypsoGisPlugin.getDefault().getPool();

  private final CachedFeatureviewFactory m_fvFactory = new CachedFeatureviewFactory( new FeatureviewHelper() );

  private final FeatureComposite m_featureComposite = new FeatureComposite( null, new FeatureSelectionManager2(), m_fvFactory );

  private final IFeatureChangeListener m_changeListener = new IFeatureChangeListener()
  {
    public void featureChanged( final FeatureChange change )
    {
      onFeatureChanged( change );
    }

    public void openFeatureRequested( final Feature feature, final IPropertyType ftp )
    {
      // feature view öffnen
      final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
      final IWorkbenchPage page = window.getActivePage();
      try
      {
        page.showView( "org.kalypso.featureview.views.FeatureView", null, IWorkbenchPage.VIEW_VISIBLE );
      }
      catch( final PartInitException e )
      {
        e.printStackTrace();
        final Shell shell = window.getShell();
        ErrorDialog.openError( shell, "Feature bearbeiten", "Fehler beim Öffnen der Feature-View", e.getStatus() );
      }

      // getLayerTable().setFocusedFeature( feature, ftp );
    }
  };

  protected Composite m_panel;

  private IPoolableObjectType m_key;

  private CommandableWorkspace m_workspace;

  private String m_featurePath;

  private Label m_label;

  private final JobExclusiveCommandTarget m_commandtarget;

  private ScrolledCompositeCreator m_creator;

  private final int m_marginWidth;

  private final int m_marginHeight;

  private boolean m_disposed = false;

  public FeatureTemplateviewer( final JobExclusiveCommandTarget commandtarget, final int marginHeight, final int marginWidth )
  {
    m_commandtarget = commandtarget;
    m_marginHeight = marginHeight;
    m_marginWidth = marginWidth;
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose( )
  {
    m_disposed = true;
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

      return StatusUtilities.statusFromThrowable( e, "Fehler beim Speichern" );
    }

    return Status.OK_STATUS;
  }

  public final Featuretemplate loadInput( final Reader reader, final URL context, final IProgressMonitor monitor, Properties props ) throws CoreException
  {
    monitor.beginTask( "Ansicht laden", 1000 );
    Featuretemplate template = null;
    try
    {
      final InputSource is = new InputSource( reader );

      final Unmarshaller unmarshaller = JC.createUnmarshaller();

      template = (Featuretemplate) unmarshaller.unmarshal( is );
      final List<FeatureviewType> view = template.getView();
      for( final FeatureviewType featureviewType : view )
        m_fvFactory.addView( featureviewType );

      final Layer layer = template.getLayer();
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
        m_featurePath = props.getProperty( "featurepath", "" );
        href = props.getProperty( "href", null );
        linktype = props.getProperty( "linktype", "gml" );
      }

      // only load, if href non null; in this case, the feature must be set via setFeature()
      if( href != null )
      {
        m_key = new PoolableObjectType( linktype, href, context );
        m_pool.addPoolListener( this, m_key );
      }
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();

      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Fehler beim Lesen der Vorlage" ) );
    }
    finally
    {
      monitor.done();
    }
    return template;
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
      public void run( )
      {
        updateControls();
      }
    } );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object,
   *      org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( final IPoolableObjectType key, final Object newValue, final IStatus status )
  {
    // Daten sind jetzt da!
    if( KeyComparator.getInstance().compare( m_key, key ) == 0 )
      setWorkspace( (CommandableWorkspace) newValue );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public void objectInvalid( final IPoolableObjectType key, final Object oldValue )
  {
    if( KeyComparator.getInstance().compare( key, m_key ) == 0 )
      setWorkspace( null );
  }

  public Composite createControls( final Composite parent, final int style )
  {
    final GridLayout gridLayout = new GridLayout();
    gridLayout.marginHeight = m_marginHeight;
    gridLayout.marginWidth = m_marginWidth;

    m_creator = new ScrolledCompositeCreator( null )
    {
      @Override
      protected Control createContents( final Composite scrollParent, final int contentStyle )
      {
        final Composite panel = new Composite( scrollParent, contentStyle );
        panel.setLayout( gridLayout );
        panel.setLayoutData( new GridData( GridData.FILL_BOTH ) );
        return panel;
      }
    };

    m_creator.createControl( parent, style | SWT.V_SCROLL, SWT.NONE );

    m_panel = (Composite) m_creator.getContentControl();

    try
    {
      updateControls();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return m_creator.getScrolledComposite();
  }

  protected void updateControls( )
  {
    final Object featureFromPath = m_workspace == null ? null : m_workspace.getFeatureFromPath( m_featurePath );
    final Feature feature = featureFromPath instanceof Feature ? (Feature) featureFromPath : null;

    final String errorMessage = "Kein Feature für Featurepath: " + m_featurePath;

    try
    {
      if( m_panel == null || m_panel.isDisposed() )
        return;

      if( m_label != null && !m_label.isDisposed() )
        m_label.dispose();

      m_featureComposite.setFeature( null );
      m_featureComposite.disposeControl();

      m_featureComposite.setFeature( null );
      m_featureComposite.updateControl();

      if( m_workspace == null )
      {
        m_label = new Label( m_panel, SWT.CENTER );
        m_label.setText( "laden..." );
        m_label.setLayoutData( new GridData( GridData.FILL_BOTH ) );
        return;
      }

      if( feature != null )
      {
        m_featureComposite.setFeature( feature );
        final Control control = m_featureComposite.createControl( m_panel, SWT.NONE, feature.getFeatureType() );
        control.setLayoutData( new GridData( GridData.FILL_BOTH ) );
        m_featureComposite.setFeature( feature );
        m_featureComposite.updateControl();

        m_panel.layout();

        m_featureComposite.addChangeListener( m_changeListener );

        m_commandtarget.resetDirty();
      }
      else
      {
        // todo Fehlermeldung anzeigen
        m_label = new Label( m_panel, SWT.CENTER );
        m_label.setText( errorMessage );
        m_label.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      final ScrolledCompositeCreator creator = m_creator;
      final ScrolledComposite scrolledComposite = m_creator.getScrolledComposite();
      if( scrolledComposite != null && !scrolledComposite.isDisposed() )
      {
        scrolledComposite.getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            if( !scrolledComposite.isDisposed() )
              creator.updateControlSize( true );
          }
        } );
      }
    }
  }

  protected void onFeatureChanged( final FeatureChange change )
  {
    m_commandtarget.postCommand( new ChangeFeaturesCommand( m_workspace, new FeatureChange[] { change } ), null );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    final FeatureComposite featureComposite = m_featureComposite;
    if( m_panel != null && !m_panel.isDisposed() )
    {
      m_panel.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          featureComposite.updateControl();
        }
      } );
    }
  }

  public Feature getFeature( )
  {
    return m_featureComposite.getFeature();
  }

  public Control getControl( )
  {
    return m_panel;
  }

  public void setFeature( final CommandableWorkspace workspace, final Feature feature )
  {
    m_featurePath = workspace.getFeaturepathForFeature( feature ).toString();
    setWorkspace( workspace );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#isDisposed()
   */
  public boolean isDisposed( )
  {
    return m_disposed;
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#dirtyChanged(org.kalypso.util.pool.IPoolableObjectType, boolean)
   */
  public void dirtyChanged( final IPoolableObjectType key, final boolean isDirty )
  {
    // TODO Auto-generated method stub
  }
}