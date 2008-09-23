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
package org.kalypso.ogc.sensor.view.wizard.cvssheet;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.sensor.view.wizard.cvssheet.CsvSheetImportDataModel.TSM_KEY;

/**
 * @author Dirk Kuch
 */
public class PageCSVDetails extends WizardPage
{

  protected final CsvSheetImportDataModel m_model;

  public PageCSVDetails( final CsvSheetImportDataModel model )
  {
    super( "pageCsvDetails" ); //$NON-NLS-1$
    m_model = model;

    setTitle( Messages.getString("org.kalypso.ogc.sensor.view.wizard.cvssheet.PageCSVDetails.1") ); //$NON-NLS-1$
    setDescription( Messages.getString("org.kalypso.ogc.sensor.view.wizard.cvssheet.PageCSVDetails.2") ); //$NON-NLS-1$
  }

  protected void checkPage( )
  {
    final String name = (String) m_model.getValue( TSM_KEY.eCsvName );
    if( (name == null) || (name.length() <= 0) )
    {
      setMessage( null );
      setErrorMessage( Messages.getString("org.kalypso.ogc.sensor.view.wizard.cvssheet.PageCSVDetails.3") ); //$NON-NLS-1$
      setPageComplete( false );

      return;
    }

    setMessage( null );
    setErrorMessage( null );
    setPageComplete( true );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    setPageComplete( false );

    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout( 2, false ) );
    container.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    setControl( container );

    /* name */
    final Label lName = new Label( container, SWT.NONE );
    lName.setText( Messages.getString("org.kalypso.ogc.sensor.view.wizard.cvssheet.PageCSVDetails.4") ); //$NON-NLS-1$

    final Text tName = new Text( container, SWT.BORDER );
    tName.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    tName.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        m_model.setValue( TSM_KEY.eCsvName, tName.getText() );

        checkPage();
      }
    } );

    /* desciption */
    final Label lDescription = new Label( container, SWT.NONE );
    lDescription.setText( Messages.getString("org.kalypso.ogc.sensor.view.wizard.cvssheet.PageCSVDetails.5") ); //$NON-NLS-1$
    lDescription.setLayoutData( new GridData( GridData.FILL, GridData.BEGINNING, false, false ) );

    final Text tDescription = new Text( container, SWT.BORDER | SWT.MULTI | SWT.WRAP );
    tDescription.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    tDescription.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        m_model.setValue( TSM_KEY.eCsvDescription, tDescription.getText() );
      }
    } );

    /* river */
    final Label lRiver = new Label( container, SWT.NONE );
    lRiver.setText( Messages.getString("org.kalypso.ogc.sensor.view.wizard.cvssheet.PageCSVDetails.6") ); //$NON-NLS-1$

    final Text tRiver = new Text( container, SWT.BORDER );
    tRiver.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    tRiver.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        m_model.setValue( TSM_KEY.eCsvRiver, tRiver.getText() );
      }
    } );

    /* river positioning */
    final Label lPosition = new Label( container, SWT.NONE );
    lPosition.setText( Messages.getString("org.kalypso.ogc.sensor.view.wizard.cvssheet.PageCSVDetails.7") ); //$NON-NLS-1$

    final Text tPosition = new Text( container, SWT.BORDER );
    tPosition.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    tPosition.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        m_model.setValue( TSM_KEY.eCsvPosition, tPosition.getText() );
      }
    } );

    checkPage();
  }

}
