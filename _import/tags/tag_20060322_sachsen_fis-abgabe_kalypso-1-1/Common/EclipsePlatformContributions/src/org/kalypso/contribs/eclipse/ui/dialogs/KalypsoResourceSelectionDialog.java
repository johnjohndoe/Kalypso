/***********************************************************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0 which accompanies this distribution, and is
 * available at http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors: IBM Corporation - initial API and implementation
 **********************************************************************************************************************/
package org.kalypso.contribs.eclipse.ui.dialogs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.resource.JFaceColors;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ISelectionValidator;
import org.eclipse.ui.dialogs.SelectionDialog;
import org.eclipse.ui.internal.ide.IDEWorkbenchMessages;

public class KalypsoResourceSelectionDialog extends SelectionDialog
{
  private IResource m_initialSelection;

  private String[] m_allowedResourceExtensions;

  private boolean m_allowNewResourceName = false;

  protected Label statusMessage;

  protected ISelectionValidator validator;

  protected ResourceSelectionValidator resourceValidator;

  private boolean m_showClosedProjects = true;

  ResourceSelectionGroup group;

  private IContainer m_inputContainer;

  /*
   * abgeleitet von ContainerSelectionDialog @author peiler
   */
  public KalypsoResourceSelectionDialog( Shell parentShell, IResource initialSelection, String message,
      String[] allowedResourceExtensions, IContainer inputContainer )
  {
    super( parentShell );
    setTitle( IDEWorkbenchMessages.getString( "Resource selection" ) ); //$NON-NLS-1$
    m_allowedResourceExtensions = allowedResourceExtensions;
    m_inputContainer = inputContainer;
    m_initialSelection = initialSelection;
    resourceValidator = new ResourceSelectionValidator();
    if( message != null )
      setMessage( message );
    else
      setMessage( IDEWorkbenchMessages.getString( "Select resource" ) ); //$NON-NLS-1$
    setShellStyle( getShellStyle() | SWT.RESIZE );
  }

  protected Control createDialogArea( Composite parent )
  {
    // create composite
    Composite area = (Composite)super.createDialogArea( parent );

    Listener listener = new Listener()
    {
      public void handleEvent( Event event )
      {
        if( statusMessage != null && validator != null )
        {
          String errorMsg = validator.isValid( group.getResourceFullPath() );
          if( errorMsg == null || errorMsg.equals( "" ) ) { //$NON-NLS-1$
            statusMessage.setText( "" ); //$NON-NLS-1$
            getOkButton().setEnabled( true );
          }
          else
          {
            statusMessage.setForeground( JFaceColors.getErrorText( statusMessage.getDisplay() ) );
            statusMessage.setText( errorMsg );
            getOkButton().setEnabled( false );
          }
        }

        if( statusMessage != null && resourceValidator != null )
        {
          if( resourceValidator.isValid( group.getSelectedResource() ) )
            getOkButton().setEnabled( true );
          else
            getOkButton().setEnabled( false );
        }
      }
    };

    statusMessage = new Label( parent, SWT.NONE );
    statusMessage.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    statusMessage.setFont( parent.getFont() );

    // container selection group
    group = new ResourceSelectionGroup( area, listener, m_allowNewResourceName, getMessage(), m_showClosedProjects,
        m_allowedResourceExtensions, m_inputContainer );

    return dialogArea;
  }

  protected Control createContents( Composite parent )
  {
    //  create the top level composite for the dialog
    Composite composite = new Composite( parent, 0 );
    GridLayout layout = new GridLayout();
    layout.marginHeight = 0;
    layout.marginWidth = 0;
    layout.verticalSpacing = 0;
    composite.setLayout( layout );
    composite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    applyDialogFont( composite );
    // initialize the dialog units
    initializeDialogUnits( composite );
    // create the dialog area and button bar
    dialogArea = createDialogArea( composite );
    buttonBar = createButtonBar( composite );

    // Change: set selected resource
    if( m_initialSelection != null )
      group.setSelectedResource( m_initialSelection );

    return composite;
  }

  /**
   * The <code>ContainerSelectionDialog</code> implementation of this <code>Dialog</code> method builds a list of
   * the selected resource containers for later retrieval by the client and closes this dialog.
   */
  protected void okPressed()
  {

    List chosenResourcePathList = new ArrayList();
    IPath returnValue = group.getResourceFullPath();
    if( returnValue != null )
      chosenResourcePathList.add( returnValue );
    setResult( chosenResourcePathList );
    super.okPressed();
  }

  /**
   * Sets the validator to use.
   * 
   * @param val
   *          A selection validator
   */
  public void setValidator( ISelectionValidator val )
  {
    this.validator = val;
  }

  /**
   * Set whether or not closed projects should be shown in the selection dialog.
   * 
   * @param show
   *          Whether or not to show closed projects.
   */
  public void showClosedProjects( boolean show )
  {
    m_showClosedProjects = show;
  }

}