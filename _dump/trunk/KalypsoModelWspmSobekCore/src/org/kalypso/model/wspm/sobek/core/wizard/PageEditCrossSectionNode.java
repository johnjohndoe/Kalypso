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
package org.kalypso.model.wspm.sobek.core.wizard;

import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.contribs.eclipse.jface.viewers.FacadeComboViewer;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.util.swt.WizardFeatureTextBox;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author kuch
 */
public class PageEditCrossSectionNode extends WizardPage
{
  protected final Feature m_node;

  private WizardFeatureTextBox m_name;

  private WizardFeatureTextBox m_description;

  protected final GMLWorkspace m_workspace;

  private FacadeComboViewer m_viewer;

  protected PageEditCrossSectionNode( final GMLWorkspace workspace, final Feature node )
  {
    super( "editFlowNetworkCrossSectionNode" );
    m_workspace = workspace;
    m_node = node;
    setTitle( "Edit cross section node" );
    setDescription( "Edit flow network cross section node" );
  }

  protected void checkPageCompleted( )
  {
    setPageComplete( false );

    if( m_name != null && "".equals( m_name.getText() ) )
    {
      setMessage( null );
      setErrorMessage( "Name is missing" );

      return;
    }

    final StructuredSelection selection = (StructuredSelection) m_viewer.getSelection();
    final Object element = selection.getFirstElement();
    if( element == null )
    {
      setMessage( null );
      setErrorMessage( "Profile selection is missing" );

      return;
    }

    setPageComplete( true );
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
    setControl( container );

    /* name */
    final Label lName = new Label( container, SWT.NONE );
    lName.setText( "Name" );

    m_name = new WizardFeatureTextBox( m_node, ISobekConstants.QN_HYDRAULIC_NAME );
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
    lDescription.setLayoutData( new GridData( GridData.FILL, GridData.BEGINNING, false, false ) );

    m_description = new WizardFeatureTextBox( m_node, ISobekConstants.QN_HYDRAULIC_DESCRIPTION );
    m_description.draw( container, new GridData( GridData.FILL, GridData.FILL, true, true ), SWT.BORDER | SWT.WRAP | SWT.MULTI );

    /* profile */
    final Label lProfile = new Label( container, SWT.NONE );
    lProfile.setText( "Cross section" );

    // TODO

// final IFCVDelegate delegate = new IFCVDelegate()
// {
// protected Set m_profiles;
//
// public ISelection getDefaultKey( )
// {
// ILinkFeatureWrapperDelegate wrapper = new ILinkFeatureWrapperDelegate()
// {
// public Feature getLinkedFeature( String id )
// {
// for( Object object : m_profiles )
// {
// if( !(object instanceof Feature) )
// continue;
//
// Feature profile = (Feature) object;
// if( profile.getId().equals( id ) )
// return profile;
// }
//
// return null;
// }
//
// // $ANALYSIS-IGNORE
// public Object getProperty( )
// {
// return m_node.getProperty( ISobekConstants.QN_HYDRAULIC_CROSS_SECTION_NODE_LINKED_PROFILE );
// }
// };
//
// LinkFeatureWrapper lnkProfile = new LinkFeatureWrapper( wrapper );
// return new StructuredSelection( lnkProfile.getFeature() );
// }
//
// // public Object[] getInputData( )
// // {
// // m_profiles = new HashSet();
// //
// // Feature root = m_pool.getWorkspace().getRootFeature();
// //
// // List< ? > waterBodies = (List< ? >) root.getProperty( GmlConstants.QN_HYDRAULIC_WATER_BODY_MEMBER );
// // for( Object object : waterBodies )
// // {
// // if( !(object instanceof Feature) )
// // continue;
// //
// // Feature waterBody = (Feature) object;
// //
// // List< ? > profiles = (List< ? >) waterBody.getProperty( GmlConstants.QN_HYDRAULIC_PROFILE_MEMBER );
// // m_profiles.addAll( profiles );
// // }
// //
// // return m_profiles.toArray();
// // }
//
// public String getValue( Object element )
// {
// Feature profile = (Feature) element;
// BigDecimal station = (BigDecimal) profile.getProperty( GmlConstants.QN_HYDRAULIC_PROFILE_STATION );
//
// return "Station: " + station.toString();
// }
// };
//
// m_viewer = new FacadeComboViewer( delegate );
// m_viewer.draw( container, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.READ_ONLY | SWT.SINGLE |
// SWT.BORDER );

    checkPageCompleted();
  }

  public String getNodeDescription( )
  {
    return m_description.getText();
  }

  public String getNodeName( )
  {
    return m_name.getText();
  }

  public Feature getNodeProfile( )
  {
    final StructuredSelection selection = (StructuredSelection) m_viewer.getSelection();

    final Object element = selection.getFirstElement();
    if( element instanceof Feature )
      return (Feature) element;

    throw new IllegalStateException();
  }
}
