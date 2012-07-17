/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.rrm.internal.cm.thiessen;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.layoutwizard.ILayoutPageContext;
import org.kalypso.core.layoutwizard.ILayoutWizardPage;
import org.kalypso.core.layoutwizard.part.AbstractLayoutPart;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.timeseries.TimeseriesValidatingOperation;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ui.rrm.internal.cm.LinearSumHelper;
import org.kalypso.ui.rrm.internal.cm.view.InitThiessenTimeseriesOperation;
import org.kalypso.ui.rrm.internal.cm.view.LinearSumBean;
import org.kalypso.ui.rrm.internal.cm.view.LinearSumNewComposite;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class ThiessenWizardLayoutPart extends AbstractLayoutPart
{
  private final PropertyChangeListener m_propertyListener = new PropertyChangeListener()
  {
    @Override
    public void propertyChange( final PropertyChangeEvent evt )
    {
      if( evt.getPropertyName().equals( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE.toString() ) )
        handleParameterTypeChanged( evt );
      else
        handlePropertyChanged();
    }
  };

  private boolean m_ignoreNextChange = false;

  private final LinearSumBean m_generator;

  private StatusComposite m_mainStatusComposite;

  public ThiessenWizardLayoutPart( final String id, final ILayoutPageContext context )
  {
    super( id, context );

    final ThiessenGeneratorWizard wizard = (ThiessenGeneratorWizard) context.getPage().getWizard();
    m_generator = wizard.getGenerator();
    m_mainStatusComposite = null;
  }

  @Override
  public void init( )
  {
  }

  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    /* Create the data binding. */
    final ILayoutWizardPage page = getContext().getPage();
    final DatabindingWizardPage binding = new DatabindingWizardPage( (WizardPage) page, toolkit );

    /* Create the main composite. */
    final Composite main = toolkit.createComposite( parent, getStyle() );
    final GridLayout panelLayout = new GridLayout( 1, false );
    panelLayout.marginHeight = 0;
    panelLayout.marginWidth = 0;
    main.setLayout( panelLayout );
    main.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create the properties section. */
    createPropertiesSection( toolkit, main, binding );

    /* Create the status composite. */
    m_mainStatusComposite = new StatusComposite( main, StatusComposite.DETAILS );
    m_mainStatusComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    /* Adapt. */
    toolkit.adapt( m_mainStatusComposite );

    /* Validate the timeseries ranges. */
    validateTimeseriesRanges( m_generator );

    /* Observe change of parameter type */
    m_generator.addPropertyChangeListener( m_propertyListener );

    return main;
  }

  @Override
  public void dispose( )
  {
    m_mainStatusComposite = null;
  }

  private void createPropertiesSection( final FormToolkit toolkit, final Composite parent, final DatabindingWizardPage binding )
  {
    /* Create the properties section. */
    final Section propertiesSection = toolkit.createSection( parent, Section.EXPANDED | Section.TITLE_BAR );
    propertiesSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    propertiesSection.setText( Messages.getString( "ThiessenWizardLayoutPart_0" ) ); //$NON-NLS-1$

    /* Create the client composite. */
    final Composite client = toolkit.createComposite( propertiesSection );
    client.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    GridLayoutFactory.fillDefaults().applyTo( client );

    /* Set the client. */
    propertiesSection.setClient( client );

    /* Create the linear sum composite. */
    createLinearSumComposite( toolkit, client, binding );
  }

  private void createLinearSumComposite( final FormToolkit toolkit, final Composite parent, final DatabindingWizardPage binding )
  {
    /* Create the form. */
    final ScrolledForm form = toolkit.createScrolledForm( parent );
    form.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    form.setExpandHorizontal( true );
    form.setExpandVertical( true );

    /* Get the body. */
    final Composite body = form.getBody();
    final GridLayout bodyLayout = new GridLayout( 1, false );
    bodyLayout.marginHeight = 0;
    bodyLayout.marginWidth = 0;
    body.setLayout( bodyLayout );

    /* Linear sum control. */
    final LinearSumNewComposite sumComposite = new LinearSumNewComposite( body, m_generator, binding, false );
    sumComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Do a reflow and a layout. */
    form.reflow( true );
    form.layout( true, true );
  }

  protected void handleParameterTypeChanged( final PropertyChangeEvent evt )
  {
    /* Avoid loop, if we cancel the change. */
    if( m_ignoreNextChange == true )
    {
      m_ignoreNextChange = false;
      return;
    }

    final ILayoutPageContext context = getContext();
    final Shell shell = context.getShell();
    final IWizard wizard = context.getPage().getWizard();
    final String windowTitle = wizard.getWindowTitle();
    final String message = Messages.getString( "ThiessenWizardLayoutPart_2" ); //$NON-NLS-1$

    if( !MessageDialog.openConfirm( shell, windowTitle, message ) )
    {
      m_ignoreNextChange = true;

      final LinearSumBean generator = m_generator;
      final Runnable revertOperation = new Runnable()
      {
        @Override
        public void run( )
        {
          generator.setProperty( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE, evt.getOldValue() );
        }
      };
      shell.getDisplay().asyncExec( revertOperation );

      return;
    }

    /* Update timeseries gml file. */
    final IWizardContainer container = wizard.getContainer();
    final ICoreRunnableWithProgress operation = new InitThiessenTimeseriesOperation( m_generator );
    final IStatus updateStatus = RunnableContextHelper.execute( container, true, false, operation );
    if( !updateStatus.isOK() )
      StatusDialog.open( shell, updateStatus, windowTitle );

    /* Validate the timeseries ranges. */
    validateTimeseriesRanges( m_generator );
  }

  protected void handlePropertyChanged( )
  {
    /* Validate the timeseries ranges. */
    validateTimeseriesRanges( m_generator );
  }

  protected void validateTimeseriesRanges( final LinearSumBean bean )
  {
    if( m_mainStatusComposite == null || m_mainStatusComposite.isDisposed() )
      return;

    final ITimeseries[] timeseries = LinearSumHelper.collectTimeseries( bean );
    final DateRange dateRange = LinearSumHelper.createDateRange( bean );

    final TimeseriesValidatingOperation operation = new TimeseriesValidatingOperation( timeseries, dateRange );
    final IStatus status = operation.execute( new NullProgressMonitor() );

    m_mainStatusComposite.setStatus( status );
  }
}