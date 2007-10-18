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
package org.kalypso.model.wspm.sobek.core.wizard.pages;

import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.contribs.eclipse.jface.viewers.FacadeComboViewer;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.util.swt.FCVFeatureDelegate;
import org.kalypso.util.swt.WizardFeatureTextBox;

/**
 * @author kuch
 */
public class PageEditBoundaryNode extends WizardPage
{

  private final IBoundaryNode m_boundaryNode;

  private WizardFeatureTextBox m_description;

  private WizardFeatureTextBox m_name;

  private FacadeComboViewer m_type;

  public PageEditBoundaryNode( final IBoundaryNode boundaryNode )
  {
    super( "editBoundaryNode" );
    m_boundaryNode = boundaryNode;
    setTitle( "Edit boundary node" );
    setDescription( "Enter boundary node parameters, please." );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    setPageComplete( false );

    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout( 2, false ) );
    setControl( container );

    /* name */
    final Label lName = new Label( container, SWT.NONE );
    lName.setText( "Name" );

    m_name = new WizardFeatureTextBox( m_boundaryNode.getFeature(), ISobekConstants.QN_HYDRAULIC_NAME );
    m_name.draw( container, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER );

    m_name.addModifyListener( new Runnable()
    {
      public void run( )
      {
        checkPageCompleted();
      }
    } );

    /* description */
    final Label lDescription = new Label( container, SWT.NONE );
    lDescription.setText( "Description" );

    m_description = new WizardFeatureTextBox( m_boundaryNode.getFeature(), ISobekConstants.QN_HYDRAULIC_DESCRIPTION );
    m_description.draw( container, new GridData( GridData.FILL, GridData.FILL, true, true ), SWT.BORDER | SWT.MULTI | SWT.WRAP );

    /* bc type */
    final Label lType = new Label( container, SWT.NONE );
    lType.setText( "Boundary Node Type" );

    m_type = new FacadeComboViewer( new FCVFeatureDelegate( m_boundaryNode.getFeature(), ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_TYPE ) );
    m_type.draw( container, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER | SWT.SINGLE | SWT.READ_ONLY );

    m_type.addSelectionChangedListener( new Runnable()
    {
      public void run( )
      {
        checkPageCompleted();
      }
    } );

    checkPageCompleted();
  }

  protected void checkPageCompleted( )
  {
    if( m_name.getText() == null )
    {
      setMessage( null );
      setErrorMessage( "Name not defined." );
      setPageComplete( false );

      return;
    }

    final StructuredSelection selection = (StructuredSelection) m_type.getSelection();
    if( selection.getFirstElement() == null )
    {
      setMessage( null );
      setErrorMessage( "Boundary Node type not set." );
      setPageComplete( false );

      return;
    }

    setMessage( null );
    setErrorMessage( null );
    setPageComplete( true );
  }

  public String getBoundaryName( )
  {
    return m_name.getText();
  }

  public String getBoundaryDescription( )
  {
    if( m_description.getText() == null )
      return "";

    return m_description.getText();
  }

  public String getBoundaryType( )
  {
    final StructuredSelection selection = (StructuredSelection) m_type.getSelection();
    return (String) selection.getFirstElement();
  }

}
