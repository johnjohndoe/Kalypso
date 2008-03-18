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
package org.kalypso.model.wspm.sobek.core.wizard.pages;

import java.util.GregorianCalendar;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.model.wspm.sobek.core.Messages;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.ui.boundarycondition.LastfallDateChooser;
import org.kalypso.util.swt.WizardFeatureLabel;
import org.kalypso.util.swt.WizardFeatureTextBox;

/**
 * @author kuch
 */
public class PageEditLastfall extends WizardPage
{
  private WizardFeatureTextBox m_name;

  private WizardFeatureTextBox m_description;

  private final ILastfall m_lastfall;

  private LastfallDateChooser m_dateBegin;

  private LastfallDateChooser m_dateEnd;

  private WizardFeatureTextBox m_pre;

  private WizardFeatureTextBox m_timeStep;

  private WizardFeatureTextBox m_multiplier;

  public PageEditLastfall( final ILastfall lastfall )
  {
    super( "createLastfall" ); //$NON-NLS-1$
    m_lastfall = lastfall;
    setTitle( Messages.PageEditLastfall_1 );
    setDescription( Messages.PageEditLastfall_2 );
  }

  protected void checkPageCompleted( )
  {
    if( (m_name.getText() == null) || "".equals( m_name.getText().trim() ) ) //$NON-NLS-1$
    {
      setMessage( null );
      setErrorMessage( Messages.PageEditLastfall_4 );
      setPageComplete( false );

      return;
    }

    if( m_dateBegin.getDateTime() == null )
    {
      setMessage( null );
      setErrorMessage( Messages.PageEditLastfall_5 );
      setPageComplete( false );

      return;
    }

    if( m_dateEnd.getDateTime() == null )
    {
      setMessage( null );
      setErrorMessage( Messages.PageEditLastfall_6 );
      setPageComplete( false );

      return;
    }

    if( m_dateEnd.getDateTime().before( m_dateBegin.getDateTime() ) )
    {
      setMessage( null );
      setErrorMessage( Messages.PageEditLastfall_7 );
      setPageComplete( false );

      return;
    }

    if( (m_pre.getText() == null) || "".equals( m_pre.getText().trim() ) ) //$NON-NLS-1$
    {
      setMessage( null );
      setErrorMessage( Messages.PageEditLastfall_9 );
      setPageComplete( false );

      return;
    }

    if( (m_timeStep.getText() == null) || "".equals( m_timeStep.getText().trim() ) ) //$NON-NLS-1$
    {
      setMessage( null );
      setErrorMessage( Messages.PageEditLastfall_11 );
      setPageComplete( false );

      return;
    }

    if( (m_multiplier.getText() == null) || "".equals( m_multiplier.getText().trim() ) ) //$NON-NLS-1$
    {
      setMessage( null );
      setErrorMessage( Messages.PageEditLastfall_13 );
      setPageComplete( false );

      return;
    }

    try
    {
      Integer.valueOf( m_pre.getText() );
    }
    catch( final NumberFormatException e )
    {
      setMessage( null );
      setErrorMessage( Messages.PageEditLastfall_14 );
      setPageComplete( false );

      return;
    }

    try
    {
      Integer.valueOf( m_timeStep.getText() );
    }
    catch( final NumberFormatException e )
    {
      setMessage( null );
      setErrorMessage( Messages.PageEditLastfall_15 );
      setPageComplete( false );

      return;
    }

    try
    {
      Integer.valueOf( m_multiplier.getText() );
    }
    catch( final NumberFormatException e )
    {
      setMessage( null );
      setErrorMessage( Messages.PageEditLastfall_16 );
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

    final Object layoutData = parent.getLayoutData();
    if( layoutData instanceof GridData )
    {
      final GridData pLayout = (GridData) layoutData;
      pLayout.widthHint = 500;
      pLayout.heightHint = 300;
      parent.layout();
    }

    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout( 2, false ) );
    setControl( container );

    /* name */
    new WizardFeatureLabel( m_lastfall.getFeature(), ISobekConstants.QN_HYDRAULIC_NAME, Messages.PageEditLastfall_17, container );

    m_name = new WizardFeatureTextBox( m_lastfall.getFeature(), ISobekConstants.QN_HYDRAULIC_NAME );
    m_name.draw( container, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER );

    m_name.addModifyListener( new Runnable()
    {
      public void run( )
      {
        checkPageCompleted();
      }
    } );

    /* description */
    new WizardFeatureLabel( m_lastfall.getFeature(), ISobekConstants.QN_HYDRAULIC_DESCRIPTION, Messages.PageEditLastfall_18, container, new GridData( GridData.FILL, GridData.BEGINNING, false, false ) );

    m_description = new WizardFeatureTextBox( m_lastfall.getFeature(), ISobekConstants.QN_HYDRAULIC_DESCRIPTION );
    m_description.draw( container, new GridData( GridData.FILL, GridData.FILL, true, true ), SWT.BORDER | SWT.MULTI | SWT.WRAP );

    final Group group = new Group( container, SWT.NONE );
    group.setLayout( new GridLayout( 2, false ) );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false, 2, 0 ) );

    /* begin date */
    new WizardFeatureLabel( m_lastfall.getFeature(), ISobekConstants.QN_LASTFALL_SIMULATION_BEGIN, Messages.PageEditLastfall_19, group );

    m_dateBegin = new LastfallDateChooser( m_lastfall.getLastfallStart() );
    m_dateBegin.draw( group, new GridData( GridData.FILL, GridData.FILL, true, false ) );

    m_dateBegin.addModifyListener( new Runnable()
    {
      public void run( )
      {
        checkPageCompleted();
      }
    } );

    /* end date */
    new WizardFeatureLabel( m_lastfall.getFeature(), ISobekConstants.QN_LASTFALL_SIMULATION_END, Messages.PageEditLastfall_20, group );

    m_dateEnd = new LastfallDateChooser( m_lastfall.getLastfallEnd() );
    m_dateEnd.draw( group, new GridData( GridData.FILL, GridData.FILL, true, false ) );

    m_dateEnd.addModifyListener( new Runnable()
    {
      public void run( )
      {
        checkPageCompleted();
      }
    } );

    /* pre simulation time */
    new WizardFeatureLabel( m_lastfall.getFeature(), ISobekConstants.QN_LASTFALL_SIMULATION_PRE_TIME, Messages.PageEditLastfall_21, group );

    m_pre = new WizardFeatureTextBox( m_lastfall.getFeature(), ISobekConstants.QN_LASTFALL_SIMULATION_PRE_TIME );
    m_pre.draw( group, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER );

    m_pre.addModifyListener( new Runnable()
    {
      public void run( )
      {
        checkPageCompleted();
      }
    } );

    /* Simulation time step */
    new WizardFeatureLabel( m_lastfall.getFeature(), ISobekConstants.QN_LASTFALL_SIMULATION_TIMESTEP, Messages.PageEditLastfall_22, group );

    m_timeStep = new WizardFeatureTextBox( m_lastfall.getFeature(), ISobekConstants.QN_LASTFALL_SIMULATION_TIMESTEP );
    m_timeStep.draw( group, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER );

    m_timeStep.addModifyListener( new Runnable()
    {
      public void run( )
      {
        checkPageCompleted();
      }
    } );

    /* Simulation time step multiplier */
    new WizardFeatureLabel( m_lastfall.getFeature(), ISobekConstants.QN_LASTFALL_SIMULATION_TIMESTEP_MULTIPLIER, Messages.PageEditLastfall_23, group );

    m_multiplier = new WizardFeatureTextBox( m_lastfall.getFeature(), ISobekConstants.QN_LASTFALL_SIMULATION_TIMESTEP_MULTIPLIER );
    m_multiplier.draw( group, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER );

    m_multiplier.addModifyListener( new Runnable()
    {
      public void run( )
      {
        checkPageCompleted();
      }
    } );

    checkPageCompleted();
  }

  public String getLastfallDescription( )
  {
    if( m_description.getText() == null )
      return ""; //$NON-NLS-1$

    return m_description.getText();
  }

  public String getLastfallName( )
  {
    return m_name.getText();
  }

  public String getLastfallPreSimulationTime( )
  {
    return m_pre.getText();
  }

  public GregorianCalendar getLastfallSimulationBegin( )
  {
    return m_dateBegin.getDateTime();
  }

  public GregorianCalendar getLastfallSimulationEnd( )
  {
    return m_dateEnd.getDateTime();
  }

  public String getLastfallSimulationTimeStep( )
  {
    return m_timeStep.getText();
  }

  public String getLastfallSimulationTimeStepMultiplier( )
  {
    return m_multiplier.getText();
  }

}
