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
package org.kalypso.model.wspm.sobek.core.wizard.pages.renderer;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.model.wspm.sobek.core.Messages;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.util.swt.WizardFeatureLabel;
import org.kalypso.util.swt.WizardFeatureTextBox;

/**
 * @author kuch
 */
public class ConstantValueComposite extends Composite
{
  private WizardFeatureTextBox m_constant;

  private WizardFeatureTextBox m_intervall;

  private final WizardPage m_page;

  public ConstantValueComposite( final WizardPage page, final Composite parent, final int style )
  {
    super( parent, style );
    m_page = page;

    final GridLayout gridLayout = new GridLayout();
    gridLayout.marginWidth = 0;
    setLayout( gridLayout );
    setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
  }

  public void render( final IBoundaryNodeLastfallCondition condition )
  {
    final Group group = new Group( this, SWT.NULL );
    group.setLayout( new GridLayout( 2, false ) );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    group.setText( Messages.PageEditBoundaryConditionTimeSeries_15 );

    /* const value */
    new WizardFeatureLabel( condition.getFeature(), ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE, group );

    m_constant = new WizardFeatureTextBox( condition.getFeature(), ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE );
    m_constant.draw( group, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER );

    m_constant.addModifyListener( new Runnable()
    {
      public void run( )
      {
        checkPageCompleted();
      }
    } );

    /* const intervall */
    new WizardFeatureLabel( condition.getFeature(), ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE_INTERVALL, group );

    m_intervall = new WizardFeatureTextBox( condition.getFeature(), ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_CONST_VALUE_INTERVALL );
    m_intervall.draw( group, new GridData( GridData.FILL, GridData.FILL, true, false ), SWT.BORDER );

    m_intervall.addModifyListener( new Runnable()
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
    if( (m_constant.getText() == null) || "".equals( m_constant.getText().trim() ) ) //$NON-NLS-1$
    {
      m_page.setMessage( null );
      m_page.setErrorMessage( Messages.PageEditBoundaryConditionTimeSeries_8 );
      m_page.setPageComplete( false );

      return;
    }
    try
    {
      Double.valueOf( m_constant.getText() );
    }
    catch( final NumberFormatException e )
    {
      m_page.setMessage( null );
      m_page.setErrorMessage( Messages.PageEditBoundaryConditionTimeSeries_9 );
      m_page.setPageComplete( false );

      return;
    }

    try
    {
      final String text = m_intervall.getText();
      if( (text != null) || !"".equals( text.trim() ) ) //$NON-NLS-1$
      {
        final Integer value = Integer.valueOf( m_intervall.getText() );

        if( value <= 0 )
        {
          m_page.setMessage( null );
          m_page.setErrorMessage( Messages.PageEditBoundaryConditionTimeSeries_11 );
          m_page.setPageComplete( false );

          return;
        }
      }

    }
    catch( final NumberFormatException e )
    {
      m_page.setMessage( null );
      m_page.setErrorMessage( Messages.PageEditBoundaryConditionTimeSeries_12 );
      m_page.setPageComplete( false );

      return;
    }

    m_page.setMessage( null );
    m_page.setErrorMessage( null );
    m_page.setPageComplete( true );

  }

  public Double getConstValue( )
  {
    return Double.valueOf( m_constant.getText().replaceAll( ",", "." ) );
  }

  public Integer getConstValueIntervall( )
  {
    try
    {
      final Integer intervall = Integer.valueOf( m_intervall.getText() );
      return intervall;
    }
    catch( final NumberFormatException e )
    {
      return null;
    }
  }

}
