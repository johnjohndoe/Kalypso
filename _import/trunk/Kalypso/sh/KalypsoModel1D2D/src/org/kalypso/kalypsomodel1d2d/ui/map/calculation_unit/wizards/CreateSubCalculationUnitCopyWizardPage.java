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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnit1D2DLabelProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * @author Madanagopal
 * 
 */
public class CreateSubCalculationUnitCopyWizardPage extends WizardPage
{
  private ArrayList<ICalculationUnit> inputListWithNo1D2D = new ArrayList<ICalculationUnit>();

  private ArrayList<ICalculationUnit> inputListCalSubUnits;

  private TableViewer subCalculationUnits;

  private Table subCalcUnitsTable;

  private ICalculationUnit1D2D calculation1D2D;

  private ICalculationUnit1D2D parentCalcUnit1D2D;

  private Composite parent;

  private Combo typeCombo;

  private CalculationUnitDataModel dataModel;

  private TableViewer calculationUnits;

  private ArrayList<ICalculationUnit> buffInputListWithNo1D2D = new ArrayList<ICalculationUnit>();

  private ArrayList<ICalculationUnit> buffInputListCalSubUnits = new ArrayList<ICalculationUnit>();

  class calculationUnitsSelectionFilter extends ViewerFilter
  {
    private ArrayList<ICalculationUnit> inputList;

    public calculationUnitsSelectionFilter( ArrayList<ICalculationUnit> inputList )
    {
      this.inputList = inputList;
    }

    public boolean select( Viewer viewer, Object parentElement, Object element )
    {
      return (!inputList.contains( (ICalculationUnit) element ));
    }
  }

  protected CreateSubCalculationUnitCopyWizardPage( CalculationUnitDataModel dataModel )
  {
    super( Messages.getString( "CreateSubCalculationUnitCopyWizardPage.0" ) ); //$NON-NLS-1$
    parentCalcUnit1D2D = (ICalculationUnit1D2D) dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    setTitle( Messages.getString( "CreateSubCalculationUnitCopyWizardPage.0" ) );
    setDescription( Messages.getString( "CreateSubCalculationUnitCopyWizardPage.1" ) + " " + parentCalcUnit1D2D.getName() );
    this.dataModel = dataModel;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    Composite comp = new Composite( parent, SWT.NULL );
    FormLayout layout = new FormLayout();
    comp.setLayout( layout );
    setControl( comp );

    FormData formData = new FormData();

    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );

    final Label name1D2D = new Label( comp, SWT.NONE );
    name1D2D.setText( Messages.getString( "CreateSubCalculationUnitCopyWizardPage.2" ) + " " + parentCalcUnit1D2D.getName()); //$NON-NLS-1$ //$NON-NLS-2$
    name1D2D.setLayoutData( formData );

    formData = new FormData();
    formData.left = new FormAttachment( name1D2D, 5 );
    formData.right = new FormAttachment( 100, -5 );
    formData.top = new FormAttachment( 0, 8 );

    formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( name1D2D, 15 );
    formData.height = 150;
    formData.right = new FormAttachment( 45, 0 );

    final Group availableCalcUnitsGroup = new Group( comp, SWT.NONE );
    availableCalcUnitsGroup.setText( Messages.getString( "CreateSubCalculationUnitCopyWizardPage.3" ) ); // NON-NLS-1$
    availableCalcUnitsGroup.setLayoutData( formData );
    availableCalcUnitsGroup.setLayout( new GridLayout( 1, false ) );

    calculationUnits = new TableViewer( availableCalcUnitsGroup );
    calculationUnits.setContentProvider( new ArrayContentProvider() );
    calculationUnits.setLabelProvider( new CalculationUnit1D2DLabelProvider() );

    Object inputData = dataModel.getData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST );

    if( inputData == null )
    {
      inputData = new ArrayList<IFeatureWrapper2>();
    }
    else
    {
      for( ICalculationUnit cUnit : (List<ICalculationUnit>) inputData )
      {
        if( (cUnit instanceof ICalculationUnit1D) || (cUnit instanceof ICalculationUnit2D) )
        {
          inputListWithNo1D2D.add( cUnit );
        }

      }
    }

    calculationUnits.setInput( inputListWithNo1D2D );

    final Table calcUnitsTable = calculationUnits.getTable();
    calcUnitsTable.setLinesVisible( true );
    calcUnitsTable.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    Button addButton = new Button( comp, SWT.PUSH );
    // addButton.setText( "ADD");
    Image addImage = new Image( comp.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/forward.gif" ).getImageData() ); //$NON-NLS-1$
    addButton.setImage( addImage );
    formData = new FormData();
    formData.left = new FormAttachment( availableCalcUnitsGroup, 5 );
    formData.top = new FormAttachment( 45 );
    // formData.right = new FormAttachment(50,0);
    addButton.setLayoutData( formData );

    addButton.addSelectionListener( new SelectionAdapter()
    {

      public void widgetSelected( SelectionEvent e )
      {
        inputListCalSubUnits.add( inputListWithNo1D2D.get( calcUnitsTable.getSelectionIndex() ) );
        inputListWithNo1D2D.remove( calcUnitsTable.getSelectionIndex() );
        subCalculationUnits.refresh();
        calculationUnits.refresh();
      }

    } );

    Button removeButton = new Button( comp, SWT.PUSH );
    // removeButton.setText( "MOVE");
    Image removeImage = new Image( comp.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/backward.gif" ).getImageData() ); //$NON-NLS-1$
    removeButton.setImage( removeImage );
    formData = new FormData();
    formData.left = new FormAttachment( availableCalcUnitsGroup, 5 );
    formData.top = new FormAttachment( addButton, 10 );
    removeButton.setLayoutData( formData );
    removeButton.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        inputListWithNo1D2D.add( (ICalculationUnit) inputListCalSubUnits.get( subCalcUnitsTable.getSelectionIndex() ) );
        inputListCalSubUnits.remove( subCalcUnitsTable.getSelectionIndex() );
        calculationUnits.refresh();
        subCalculationUnits.refresh();
      }
    } );

    formData = new FormData();
    formData.left = new FormAttachment( removeButton, 5 );
    formData.top = new FormAttachment( name1D2D, 15 );
    formData.height = 150;
    formData.right = new FormAttachment( 100, -5 );

    final Group selectedSubUnitsGroup = new Group( comp, SWT.NONE );
    selectedSubUnitsGroup.setText( Messages.getString( "CreateSubCalculationUnitCopyWizardPage.4" ) ); //$NON-NLS-1$
    selectedSubUnitsGroup.setLayoutData( formData );
    selectedSubUnitsGroup.setLayout( new GridLayout( 1, false ) );

    subCalculationUnits = new TableViewer( selectedSubUnitsGroup );
    subCalculationUnits.setContentProvider( new ArrayContentProvider() );
    subCalculationUnits.setLabelProvider( new CalculationUnit1D2DLabelProvider() );

    if( parentCalcUnit1D2D.getSubUnits() == null )
    {
      subCalculationUnits.setInput( new Object[] {} );
    }
    else
    {
      inputListCalSubUnits = new ArrayList<ICalculationUnit>( parentCalcUnit1D2D.getSubUnits() );// (IFeatureWrapperCollection)
      // parentCalcUnit1D2D.getSubUnits();
      subCalculationUnits.setInput( inputListCalSubUnits );
    }

    subCalcUnitsTable = subCalculationUnits.getTable();
    subCalcUnitsTable.setLinesVisible( true );
    subCalcUnitsTable.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    calculationUnitsSelectionFilter unitsSelectionFilter = new calculationUnitsSelectionFilter( inputListCalSubUnits );

    if( inputListCalSubUnits.size() != 0 )
    {

      for( ICalculationUnit ele : inputListCalSubUnits )
      {
        if( inputListWithNo1D2D.contains( ele ) )
        {
          inputListWithNo1D2D.remove( ele );
        }
      }
      calculationUnits.refresh();

    }

    buffInputListWithNo1D2D.addAll( inputListWithNo1D2D );
    buffInputListCalSubUnits.addAll( inputListCalSubUnits );
  }

  public void init( IStructuredSelection selection )
  {
    // TODO Auto-generated method stub

  }

  public ArrayList<ICalculationUnit> getInputListCalSubUnits( )
  {
    return inputListCalSubUnits;
  }

  public ArrayList<ICalculationUnit> getInputListWithNo1D2D( )
  {
    return inputListWithNo1D2D;
  }

}
