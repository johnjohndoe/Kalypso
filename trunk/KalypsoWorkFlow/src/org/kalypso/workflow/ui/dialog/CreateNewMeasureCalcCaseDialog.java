/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.workflow.ui.dialog;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * @author kuepfer
 */
public class CreateNewMeasureCalcCaseDialog extends TitleAreaDialog
{

  private Composite m_main;

  private Composite m_top;

  private Text m_calcCaseNameText;

  private final IContainer m_container;

  private IFolder m_folderHandel;

  private Text m_calcCaseEditorText;

  private StyledText m_calcCaseDescription;

  private Button m_calcCasePlanning;

  private Button m_calcCaseMeasure;

  public CreateNewMeasureCalcCaseDialog( final Shell parentShell, final IContainer project )
  {
    super( parentShell );
    m_container = project;
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( Composite parent )
  {
    m_main = (Composite) super.createDialogArea( parent );
    m_top = new Composite( m_main, SWT.NONE );

    m_top.setLayout( new GridLayout() );
    GridData data2 = new GridData( GridData.FILL_BOTH );
    data2.grabExcessHorizontalSpace = true;
    data2.grabExcessVerticalSpace = true;
    m_top.setLayoutData( data2 );
    final Group calcCaseGroup = new Group( m_top, SWT.NULL );
    calcCaseGroup.setText( "Rechenfall anlegen:" );
    final GridLayout gridLayout = new GridLayout( 2, true );
    calcCaseGroup.setLayout( gridLayout );
    GridData data1 = new GridData( GridData.FILL_BOTH );
    data1.widthHint = 200;
    calcCaseGroup.setLayoutData( data1 );
    final Label calcCaseLabel = new Label( calcCaseGroup, SWT.NULL );
    calcCaseLabel.setText( "Name" );
    m_calcCaseNameText = new Text( calcCaseGroup, SWT.SINGLE );
    m_calcCaseNameText.addFocusListener( new FocusAdapter()
    {

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( FocusEvent e )
      {
        validate();
      }

    } );
    m_calcCaseNameText.setText( "NeuerRechenfall" );
    Label calcCaseEditorLabel = new Label( calcCaseGroup, SWT.NULL );
    calcCaseEditorLabel.setText( "Ersteller:" );
    m_calcCaseEditorText = new Text( calcCaseGroup, SWT.SINGLE );
    Label calcCaseDescriptionLabel = new Label( calcCaseGroup, SWT.NONE );
    calcCaseDescriptionLabel.setText( "Beschreibung:" );
    m_calcCaseDescription = new StyledText( calcCaseGroup, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
    m_calcCasePlanning = new Button( calcCaseGroup, SWT.RADIO );
    m_calcCasePlanning.setText( "Planvariante" );
    m_calcCaseMeasure = new Button( calcCaseGroup, SWT.RADIO );
    m_calcCaseMeasure.setText( "Maßnahmenvariante" );
    return m_main;
  }

  protected void validate( )
  {
    final Button okButton = getButton( IDialogConstants.OK_ID );
    okButton.setEnabled( false );
    String name = m_calcCaseNameText.getText();
    if( name == null || name.length() == 0 )
    {
      setErrorMessage( "Das Feld Name darf nicht leer sein. Bitte geben sie einen für die neue Rechenvariante ein." );
      return;
    }
    else
    {

      final IFolder folder = m_container.getFolder( new Path( name ) );
      if( folder.exists() )
      {
        setErrorMessage( "Die Rechenvariante besteht bereits. Bitte geben sie einen andern Name ein" );
        return;
      }
      else
      {
        m_folderHandel = folder;
      }
    }
    setErrorMessage( null );
    okButton.setEnabled( true );

  }

  public IFolder getCalcCaseFolter( ) throws CoreException
  {
    m_folderHandel.create( false, false, null );
    return m_folderHandel;
  }

  public boolean isMeasure( )
  {
    return m_calcCaseMeasure.getSelection();
  }

  public boolean isPlanning( )
  {
    return m_calcCasePlanning.getSelection();
  }
}
