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
package org.kalypso.ui.wizard.calccase;

import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.eclipse.core.resources.IProjectProvider;
import org.kalypso.eclipse.util.SetContentHelper;
import org.kalypso.java.lang.CatchRunnable;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.nature.ModelNature;

/**
 * Wizard-Page zur Eingabe der Steuerparameter
 * 
 * @author belger
 */
public class SteuerparameterWizardPage extends WizardPage
{
  private final IProjectProvider m_projectProvider;

  private final FeatureComposite m_featureComposite = new FeatureComposite( null, new URL[] {} );

  private boolean m_overrideCanFlipToNextPage;

  private boolean m_update;

  private Button m_checkUpdate;

  private GMLWorkspace m_workspace;

  private IFolder m_currentCalcCase;

  private Composite m_panel;

  public SteuerparameterWizardPage( final IProjectProvider pp,
      final boolean overrideCanFlipToNextPage, final ImageDescriptor image )
  {
    super( "EditCalcCaseControlPage", "Steuerparameter", image );

    m_projectProvider = pp;
    m_overrideCanFlipToNextPage = overrideCanFlipToNextPage;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    m_panel = new Composite( parent, SWT.NONE );
    m_panel.setLayout( new GridLayout() );
    m_panel.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    createFeatureControl( m_panel );

    setControl( m_panel );
  }

  public void saveChanges( final IFolder folder, final IProgressMonitor monitor )
      throws CoreException
  {
    monitor.beginTask( "Steuerparameter speichern", 2000 );

    // COMMITTEN
    final Collection changes = new ArrayList();

    final FeatureComposite fc = m_featureComposite;

    getControl().getDisplay().syncExec( new CatchRunnable()
    {
      public void runIntern() throws Exception
      {
        fc.collectChanges( changes );

        // �nderungen committen
        new ChangeFeaturesCommand( null, (FeatureChange[])changes
            .toArray( new FeatureChange[changes.size()] ) ).process();
      }
    } );

    monitor.worked( 1000 );

    // SPEICHERN
    final IFile controlFile = folder.getFile( ModelNature.CONTROL_NAME );

    final GMLWorkspace workspace = m_workspace;
    final SetContentHelper thread = new SetContentHelper(  )
    {
      public void write( final Writer w ) throws Throwable
      {
        GmlSerializer.serializeWorkspace( w, workspace );
      }
    };
    
    try
    {
      thread.setFileContents(controlFile, false, false, new NullProgressMonitor());
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#canFlipToNextPage()
   */
  public boolean canFlipToNextPage()
  {
    if( m_overrideCanFlipToNextPage )
      return isPageComplete();

    return super.canFlipToNextPage();
  }

  public boolean isUpdate()
  {
    return m_update;
  }

  public void setUpdate( final boolean update )
  {
    if( m_update != update )
    {
      m_update = update;

      final Button checkUpdate = m_checkUpdate;
      if( checkUpdate != null && !checkUpdate.isDisposed() )
        checkUpdate.getDisplay().asyncExec( new Runnable()
        {
          public void run()
          {
            checkUpdate.setSelection( update );
          }
        } );
    }
  }

  /**
   * Setzt die aktuelle Rechenvariante, ist dort schon eine .calculation vorhanden,
   * wird diese geladen, sonst die default.
   * 
   * @param currentCalcCase
   */
  public void setFolder( final IFolder currentCalcCase )
  {
    m_currentCalcCase = currentCalcCase;

    final Composite panel = m_panel;
    if( panel != null && !panel.isDisposed() )
    {
      m_panel.getDisplay().asyncExec( new Runnable( ) {
        public void run()
        {
          createFeatureControl( panel );
          panel.layout();
        }} );
    }
  }

  protected void createFeatureControl( final Composite panel )
  {
    // dispose old control
    m_featureComposite.disposeControl();
    if( m_checkUpdate != null )
    {
      m_checkUpdate.dispose();
      m_checkUpdate = null;
    }
    final IProject project = m_projectProvider.getProject();
    if( project == null )
      return;

    try
    {
      // gleich mal den Workspace auf das default setzen
      final ModelNature nature = (ModelNature)project.getNature( ModelNature.ID );
      m_workspace = nature.loadOrCreateControl( m_currentCalcCase );

      // Vorlage auslesen
      final URL viewURL = new URL( "platform:/resource/" + project.getName() + "/"
          + ModelNature.CONTROL_VIEW_PATH );

      final Feature f = m_workspace.getRootFeature();

      m_featureComposite.setFeature( f );
      m_featureComposite.addView( viewURL );

      final Control featureControl = m_featureComposite.createControl( panel, SWT.NONE );
      featureControl.setLayoutData( new GridData( GridData.FILL_BOTH ) );

      final Button checkUpdate = new Button( m_panel, SWT.CHECK );
      checkUpdate.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
      checkUpdate.setText( "Zeitreihen aktualisieren" );
      checkUpdate
          .setToolTipText( "falls aktiv, werden die Zeitreihen im n�chsten Schritt aktualisiert" );
      checkUpdate.setSelection( m_update );
      m_checkUpdate = checkUpdate;

      checkUpdate.addSelectionListener( new SelectionAdapter()
      {
        /**
         * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
         */
        public void widgetSelected( final SelectionEvent e )
        {
          setUpdate( checkUpdate.getSelection() );
        }
      } );
      
      final FeatureComposite featureComposite = m_featureComposite;
      if( featureComposite != null )
      {
        featureComposite.setFeature( m_workspace.getRootFeature() );
        featureComposite.updateControl();
      }
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#getPreviousPage()
   */
  public IWizardPage getPreviousPage()
  {
    return null;
  }

}