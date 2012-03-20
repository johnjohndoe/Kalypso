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
package org.kalypso.ui.rrm.internal.cm.idw;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.layoutwizard.ILayoutPageContext;
import org.kalypso.core.layoutwizard.ILayoutWizardPage;
import org.kalypso.core.layoutwizard.part.AbstractLayoutPart;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.ui.rrm.internal.cm.view.InitThiessenTimeseriesOperation;
import org.kalypso.ui.rrm.internal.cm.view.LinearSumBean;
import org.kalypso.ui.rrm.internal.cm.view.LinearSumNewComposite;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Gernot Belger
 * @author Holger Albert
 */
public class IdwWizardLayoutPart extends AbstractLayoutPart
{
  private final PropertyChangeListener m_propertyListener = new PropertyChangeListener()
  {
    @Override
    public void propertyChange( final PropertyChangeEvent evt )
    {
      handleParameterTypeChanged( evt );
    }
  };

  private boolean m_ignoreNextChange = false;

  private final LinearSumBean m_generator;

  public IdwWizardLayoutPart( final String id, final ILayoutPageContext context )
  {
    super( id, context );

    final IdwGeneratorWizard wizard = (IdwGeneratorWizard) context.getPage().getWizard();
    m_generator = wizard.getGenerator();
  }

  @Override
  public void init( )
  {
  }

  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    final ILayoutWizardPage page = getContext().getPage();
    final DatabindingWizardPage binding = new DatabindingWizardPage( (WizardPage) page, toolkit );

    final Composite panel = toolkit.createComposite( parent, getStyle() );
    GridLayoutFactory.fillDefaults().applyTo( panel );

    final Section propertiesSection = toolkit.createSection( panel, Section.EXPANDED | Section.TITLE_BAR );
    propertiesSection.setText( Messages.getString( "IdwWizardLayoutPart_0" ) ); //$NON-NLS-1$
    propertiesSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final Composite body = toolkit.createComposite( propertiesSection );
    propertiesSection.setClient( body );
    GridLayoutFactory.fillDefaults().applyTo( body );

    /* Linear sum control. */
    final LinearSumNewComposite sumComposite = new LinearSumNewComposite( body, m_generator, binding, false );
    sumComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    toolkit.createLabel( body, StringUtils.EMPTY, SWT.NONE );

    /* Header for gis table below. */
    final Section tableSection = toolkit.createSection( panel, Section.EXPANDED | Section.TITLE_BAR );
    tableSection.setText( Messages.getString( "IdwWizardLayoutPart_1" ) ); //$NON-NLS-1$
    tableSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    /* Observe change of parameter type. */
    m_generator.addPropertyChangeListener( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE.toString(), m_propertyListener );

    return panel;
  }

  @Override
  public void dispose( )
  {
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
    final String message = Messages.getString( "IdwWizardLayoutPart_2" ); //$NON-NLS-1$

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
    {
      StatusDialog.open( shell, updateStatus, windowTitle );
      return;
    }
  }
}