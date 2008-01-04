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

import java.text.DateFormat;
import java.util.GregorianCalendar;

import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.contribs.eclipse.jface.viewers.FacadeComboViewer;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode.BOUNDARY_TYPE;
import org.kalypso.model.wspm.sobek.core.ui.boundarycondition.LastfallDateChooser;
import org.kalypso.util.swt.FCVFeatureDelegate;
import org.kalypso.util.swt.WizardFeatureLabel;
import org.kalypso.util.swt.WizardFeatureTextBox;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuch
 */
public class PageEditBoundaryConditionGeneral extends WizardPage implements IBoundaryConditionGeneral
{
  private final IBoundaryNodeLastfallCondition m_condition;

  private LastfallDateChooser m_tsBegins;

  private LastfallDateChooser m_tsEnds;

  public PageEditBoundaryConditionGeneral( final IBoundaryNodeLastfallCondition condition )
  {
    super( "editBoundaryConditionGeneral" );
    m_condition = condition;

    setTitle( "Edit boundary condition" );
    setDescription( "Enter boundary condition parameters, please." );
  }

  protected void checkPageCompleted( )
  {
    if( m_tsBegins.getDateTime() == null )
    {
      setMessage( null );
      setErrorMessage( "Start date not defined" );
      setPageComplete( false );

      return;
    }

    if( m_tsEnds.getDateTime() == null )
    {
      setMessage( null );
      setErrorMessage( "End date not defined" );
      setPageComplete( false );

      return;
    }

    if( m_tsEnds.getDateTime().before( m_tsBegins.getDateTime() ) )
    {
      setMessage( null );
      setErrorMessage( "End date is before start date" );
      setPageComplete( false );

      return;
    }

    /* check lastfall start and ending dates */
    final Feature lastfall = m_condition.getLastfall().getFeature();
    final XMLGregorianCalendar lastfallStart = (XMLGregorianCalendar) lastfall.getProperty( ISobekConstants.QN_LASTFALL_SIMULATION_BEGIN );
    final XMLGregorianCalendar lastfallEnd = (XMLGregorianCalendar) lastfall.getProperty( ISobekConstants.QN_LASTFALL_SIMULATION_END );

    final GregorianCalendar lastfallGregorianStart = lastfallStart.toGregorianCalendar();
    final GregorianCalendar lastfallGregorianEnd = lastfallEnd.toGregorianCalendar();

    final DateFormat df = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.MEDIUM );

    if( m_tsBegins.getDateTime().after( lastfallGregorianStart ) )
    {
      setMessage( null );
      setErrorMessage( "Boundary condition starting date is after lastfall starting date! (" + df.format( lastfallGregorianStart.getTime() ) + ")" );
      setPageComplete( false );

      return;
    }

    if( m_tsEnds.getDateTime().before( lastfallGregorianEnd ) )
    {
      setMessage( null );
      setErrorMessage( "Boundary condition ending date is before lastfall ending date! (" + df.format( lastfallGregorianEnd.getTime() ) + ")" );
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
    setControl( container );

    final Group iGroup = new Group( container, SWT.NONE );
    iGroup.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false, 2, 0 ) );
    iGroup.setLayout( new GridLayout( 2, false ) );
    iGroup.setText( "Info" );

    /* lastfall */
    new WizardFeatureLabel( "Calculation case", iGroup );

    final WizardFeatureTextBox lf = new WizardFeatureTextBox( m_condition.getLastfall().getFeature(), ISobekConstants.QN_HYDRAULIC_NAME );
    lf.draw( iGroup, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER | SWT.READ_ONLY );
    lf.setEnabled( false );

    /* boundary node */
    new WizardFeatureLabel( "Boundary node", iGroup );

    final WizardFeatureTextBox bn = new WizardFeatureTextBox( m_condition.getBoundaryNode().getFeature(), ISobekConstants.QN_HYDRAULIC_NAME );
    bn.draw( iGroup, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER | SWT.READ_ONLY );
    bn.setEnabled( false );

    /* bc type */
    new WizardFeatureLabel( "Type of boundary node", iGroup );

    final FacadeComboViewer bt = new FacadeComboViewer( new FCVFeatureDelegate( m_condition.getBoundaryNode().getFeature(), ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_TYPE ) );
    bt.draw( iGroup, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER | SWT.SINGLE | SWT.READ_ONLY );
    bt.setEnabled( false );

    /* begin date */
    new WizardFeatureLabel( m_condition.getLastfall().getFeature(), ISobekConstants.QN_LASTFALL_SIMULATION_BEGIN, "Start date", container );

    /* newly created boundary condition? */
    if( m_condition.getObservationStart() == null )
      m_tsBegins = new LastfallDateChooser( m_condition.getLastfall().getLastfallStart() );
    else
      m_tsBegins = new LastfallDateChooser( m_condition.getObservationStart() );

    m_tsBegins.draw( container, new GridData( GridData.FILL, GridData.FILL, true, false ) );

    m_tsBegins.addModifyListener( new Runnable()
    {
      public void run( )
      {
        checkPageCompleted();
      }
    } );

    /* end date */
    /* newly created boundary condition? */
    new WizardFeatureLabel( m_condition.getLastfall().getFeature(), ISobekConstants.QN_LASTFALL_SIMULATION_END, "End date", container );

    if( m_condition.getObservationEnd() == null )
      m_tsEnds = new LastfallDateChooser( m_condition.getLastfall().getLastfallEnd() );
    else
      m_tsEnds = new LastfallDateChooser( m_condition.getObservationEnd() );
    m_tsEnds.draw( container, new GridData( GridData.FILL, GridData.FILL, true, false ) );

    m_tsEnds.addModifyListener( new Runnable()
    {
      public void run( )
      {
        checkPageCompleted();
      }
    } );

    checkPageCompleted();
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.wizard.pages.IBoundaryConditionGeneral#getBoundaryNodeType()
   */
  public BOUNDARY_TYPE getBoundaryNodeType( )
  {
    return m_condition.getBoundaryNode().getBoundaryType();
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.wizard.pages.IBoundaryConditionGeneral#getEndDate()
   */
  public GregorianCalendar getEndDate( )
  {
    return m_tsEnds.getDateTime();
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.wizard.pages.IBoundaryConditionGeneral#getStartDate()
   */
  public GregorianCalendar getStartDate( )
  {
    return m_tsBegins.getDateTime();
  }

}
