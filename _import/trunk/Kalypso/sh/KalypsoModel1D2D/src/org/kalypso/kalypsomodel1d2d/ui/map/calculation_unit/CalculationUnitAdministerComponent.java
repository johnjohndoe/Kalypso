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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit;

import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.ui.map.IWidgetWithStrategy;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CreateSubCalculationUnitCopyWizard;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;

/**
 * @author Madanagopal
 * 
 */
public class CalculationUnitAdministerComponent
{
  private Composite m_parent;

  private Composite m_rootComposite;

  private CalculationUnitDataModel m_dataModel;

  private Button m_btnAddRemoveElements;

  private Button m_btnAddRemoveContinuityLines;

  private Button m_btnAddRemoveBoundaryConditions;

  private Button m_btnAddRemoveSubUnits;

  private static final ImageData IMAGEDATA_ADDREMOVE_ELEMENTS_1D = KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/01_add_1D.gif" ).getImageData();

  private static final ImageData IMAGEDATA_ADDREMOVE_ELEMENTS_2D = KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/02_add_2D.gif" ).getImageData();

  private static final ImageData IMAGEDATA_ADDREMOVE_CONTILINES = KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/03_add_conti.gif" ).getImageData();

  private static final ImageData IMAGEDATA_ADDREMOVE_BOUNDARYCONDITIONS = KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/13_flowrelation_add_NodalBC.gif" ).getImageData();

  private static final ImageData IMAGEDATA_ADDREMOVE_SUBUNITS = KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/18_add_calculationunit.gif" ).getImageData();

  public CalculationUnitAdministerComponent( final CalculationUnitDataModel model )
  {
    m_dataModel = model;
  }

  public void createControl( final Composite parent )
  {
    m_parent = parent;
    m_rootComposite = new Composite( m_parent, SWT.FLAT );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 4;
    gridLayout.makeColumnsEqualWidth = true;
    m_rootComposite.setLayout( gridLayout );

    m_btnAddRemoveElements = new Button( m_rootComposite, SWT.PUSH );
    m_btnAddRemoveElements.setLayoutData( new GridData( GridData.BEGINNING ) );
    m_btnAddRemoveElements.setImage( new Image( m_rootComposite.getDisplay(), IMAGEDATA_ADDREMOVE_ELEMENTS_1D ) );
    m_btnAddRemoveElements.setToolTipText( "Add/remove elements to calculation unit" );
    m_btnAddRemoveElements.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        final IWidgetWithStrategy widgetWithStrategy = (IWidgetWithStrategy) m_dataModel.getData( ICommonKeys.WIDGET_WITH_STRATEGY );
        widgetWithStrategy.setStrategy( new AddRemoveElementToCalcUnitWidget( m_dataModel ) );
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        // TODO Auto-generated method stub
      }
    } );

    m_btnAddRemoveContinuityLines = new Button( m_rootComposite, SWT.PUSH );
    m_btnAddRemoveContinuityLines.setLayoutData( new GridData( GridData.BEGINNING ) );
    m_btnAddRemoveContinuityLines.setImage( new Image( m_rootComposite.getDisplay(), IMAGEDATA_ADDREMOVE_CONTILINES ) );
    m_btnAddRemoveContinuityLines.setToolTipText( "Add/remove continuity line to calculation unit" );
    m_btnAddRemoveContinuityLines.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        final IWidgetWithStrategy widgetWithStrategy = (IWidgetWithStrategy) m_dataModel.getData( ICommonKeys.WIDGET_WITH_STRATEGY );
        widgetWithStrategy.setStrategy( new AddRemoveContinuityLineToCalcUnitWidget( m_dataModel ) );
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        // TODO Auto-generated method stub
      }
    } );

    m_btnAddRemoveBoundaryConditions = new Button( m_rootComposite, SWT.PUSH );
    m_btnAddRemoveBoundaryConditions.setLayoutData( new GridData( GridData.BEGINNING ) );
    m_btnAddRemoveBoundaryConditions.setImage( new Image( m_rootComposite.getDisplay(), IMAGEDATA_ADDREMOVE_BOUNDARYCONDITIONS ) );
    m_btnAddRemoveBoundaryConditions.setToolTipText( "Add/remove boundary condition to calculation unit" );
    m_btnAddRemoveBoundaryConditions.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        final IWidgetWithStrategy widgetWithStrategy = (IWidgetWithStrategy) m_dataModel.getData( ICommonKeys.WIDGET_WITH_STRATEGY );
        widgetWithStrategy.setStrategy( new AddRemoveBoundaryConditionToCalcUnitWidget( m_dataModel ) );
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        // TODO Auto-generated method stub
      }
    } );

    m_btnAddRemoveSubUnits = new Button( m_rootComposite, SWT.PUSH );
    m_btnAddRemoveSubUnits.setLayoutData( new GridData( GridData.BEGINNING ) );
    m_btnAddRemoveSubUnits.setImage( new Image( m_rootComposite.getDisplay(), IMAGEDATA_ADDREMOVE_SUBUNITS ) );
    m_btnAddRemoveSubUnits.setToolTipText( "Add/remove boundary sub units to calculation unit" );
    m_btnAddRemoveSubUnits.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( SelectionEvent e )
      {
        final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
        final CreateSubCalculationUnitCopyWizard calculationSubWizard = new CreateSubCalculationUnitCopyWizard( m_dataModel );
        final WizardDialog wizardDialog = new WizardDialog( shell, calculationSubWizard );
        wizardDialog.open();
      }

      public void widgetDefaultSelected( SelectionEvent e )
      {
        // TODO Auto-generated method stub
      }
    } );

    m_btnAddRemoveElements.setEnabled( false );
    m_btnAddRemoveContinuityLines.setEnabled( false );
    m_btnAddRemoveBoundaryConditions.setEnabled( false );
    m_btnAddRemoveSubUnits.setEnabled( false );

    m_dataModel.addKeyBasedDataChangeListener( new KeyBasedDataModelChangeListener()
    {
      @SuppressWarnings("synthetic-access")
      public void dataChanged( final String key, final Object newValue )
      {
        final Display display = m_parent.getDisplay();
        final Runnable runnable = new Runnable()
        {
          public void run( )
          {
            if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) )
            {
              if( newValue != null )
              {
                if( newValue instanceof ICalculationUnit1D )
                {
                  m_btnAddRemoveElements.setImage( new Image( m_rootComposite.getDisplay(), IMAGEDATA_ADDREMOVE_ELEMENTS_1D ) );
                  m_btnAddRemoveElements.setEnabled( true );
                  m_btnAddRemoveContinuityLines.setEnabled( true );
                  m_btnAddRemoveBoundaryConditions.setEnabled( true );
                  m_btnAddRemoveSubUnits.setEnabled( false );
                }
                else if( newValue instanceof ICalculationUnit2D )
                {
                  m_btnAddRemoveElements.setImage( new Image( m_rootComposite.getDisplay(), IMAGEDATA_ADDREMOVE_ELEMENTS_2D ) );
                  m_btnAddRemoveElements.setEnabled( true );
                  m_btnAddRemoveContinuityLines.setEnabled( true );
                  m_btnAddRemoveBoundaryConditions.setEnabled( true );
                  m_btnAddRemoveSubUnits.setEnabled( false );
                }
                else if( newValue instanceof ICalculationUnit1D2D )
                {
                  m_btnAddRemoveElements.setEnabled( false );
                  m_btnAddRemoveContinuityLines.setEnabled( true );
                  m_btnAddRemoveBoundaryConditions.setEnabled( true );
                  m_btnAddRemoveSubUnits.setEnabled( true );
                }
                else
                {
                  m_btnAddRemoveElements.setEnabled( false );
                  m_btnAddRemoveContinuityLines.setEnabled( false );
                  m_btnAddRemoveBoundaryConditions.setEnabled( false );
                  m_btnAddRemoveSubUnits.setEnabled( false );
                }
              }
            }
          }
        };
        display.syncExec( runnable );
      }
    } );
  }

}
