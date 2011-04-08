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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys.model;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.util.Date;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IPageChangingListener;
import org.eclipse.jface.dialogs.PageChangingEvent;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;

/**
 * @author Gernot Belger
 */
public class CreateModelTinWizardPage extends WizardPage
{
  private final IPageChangingListener m_pageChangeingListener = new IPageChangingListener()
  {
    @Override
    public void handlePageChanging( final PageChangingEvent event )
    {
      doHandlePageChangeing( event );
    }
  };

  private final IFEDiscretisationModel1d2d m_model;

  private final IFile m_targetFile;

  private IStatus m_execute;

  private BigDecimal m_max;

  private BigDecimal m_min;

  private boolean m_doUpdate = true;

  private StatusComposite m_statusComposite;

  public CreateModelTinWizardPage( final String pageName, final IFile targetFile, final IFEDiscretisationModel1d2d model )
  {
    super( pageName, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.CreateModelTinWizardPage.1"), null ); //$NON-NLS-1$

    m_targetFile = targetFile;
    m_model = model;

    final String msg;
    if( m_targetFile.exists() )
    {
      final Date date = new Date( m_targetFile.getLocalTimeStamp() );
      final DateFormat df = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.SHORT );
      df.setTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() );
      final String dateString = df.format( date );
      msg =  Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.CreateModelTinWizardPage.0", dateString, IDialogConstants.NEXT_LABEL ); //$NON-NLS-1$
    }
    else
      msg =  Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.CreateModelTinWizardPage.2", IDialogConstants.NEXT_LABEL ); //$NON-NLS-1$

    m_execute = StatusUtilities.createStatus( IStatus.INFO, msg, null );

    setDescription( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.CreateModelTinWizardPage.3") ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setLayout( new GridLayout() );

    m_statusComposite = new StatusComposite( group, StatusComposite.DETAILS );
    m_statusComposite.setStatus( m_execute );
    m_statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    if( m_targetFile.exists() )
    {
      final Button button = new Button( group, SWT.CHECK );
      button.setSelection( m_doUpdate );
      button.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.CreateModelTinWizardPage.4") ); //$NON-NLS-1$
      button.setToolTipText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.CreateModelTinWizardPage.5") ); //$NON-NLS-1$
      button.addSelectionListener( new SelectionAdapter()
      {
        /**
         * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
         */
        @Override
        public void widgetSelected( final SelectionEvent e )
        {
          setDoUpdate( button.getSelection() );
        }
      } );
    }


    setControl( group );
  }

  protected void setDoUpdate( final boolean selection )
  {
    m_doUpdate = selection;
  }

  protected void doHandlePageChangeing( final PageChangingEvent event )
  {
    if( !m_doUpdate )
      return;

    if( m_execute.isOK() )
      return;

    if( event.getCurrentPage() == this && event.getTargetPage() == getNextPage() )
    {
      final ModelTinExporter tinExporter = new ModelTinExporter( m_targetFile, m_model );
      m_execute = RunnableContextHelper.execute( getContainer(), true, true, tinExporter );
      if( m_execute.isOK() )
      {
        m_min = tinExporter.getMin();
        m_max = tinExporter.getMax();
        m_statusComposite.setStatus( StatusUtilities.createStatus( IStatus.OK, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.CreateModelTinWizardPage.6"), null ) ); //$NON-NLS-1$
      }
      else
      {
        event.doit = false;
        setErrorMessage( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.CreateModelTinWizardPage.7") ); //$NON-NLS-1$
        setPageComplete( false );
        m_statusComposite.setStatus( m_execute );
      }

    }
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#setWizard(org.eclipse.jface.wizard.IWizard)
   */
  @Override
  public void setWizard( final IWizard newWizard )
  {
    final IWizard oldWizard = getWizard();
    final IWizardContainer oldContainer = oldWizard == null ? null : oldWizard.getContainer();
    if( oldContainer instanceof WizardDialog )
      ((WizardDialog) oldContainer).removePageChangingListener( m_pageChangeingListener );

    super.setWizard( newWizard );

    final IWizardContainer newContainer = newWizard == null ? null : newWizard.getContainer();
    if( newContainer instanceof WizardDialog )
      ((WizardDialog) newContainer).addPageChangingListener( m_pageChangeingListener );
  }

  public BigDecimal getMin( )
  {
    return m_min;
  }

  public BigDecimal getMax( )
  {
    return m_max;
  }
}
