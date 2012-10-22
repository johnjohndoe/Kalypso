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
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnit1D2DLabelProvider;

/**
 * @author Madanagopal
 * @author Gernot Belger
 */
public class CalculationUnitPropertyWizardPage extends WizardPage
{
  private final List<ICalculationUnit> m_selectedSubUnits = new ArrayList<>();

  private final ICalculationUnit m_parentCalcUnit;

  private final ICalculationUnit[] m_availableCalcUnits;

  protected String m_name;

  protected int m_interpolationCount;

  public CalculationUnitPropertyWizardPage( final ICalculationUnit calcUnit, final ICalculationUnit[] availableCalcUnits )
  {
    super( "" );  //$NON-NLS-1$

    m_parentCalcUnit = calcUnit;
    m_availableCalcUnits = availableCalcUnits;

    m_name = m_parentCalcUnit.getName();

    if( m_parentCalcUnit instanceof ICalculationUnit1D )
      m_interpolationCount = ((ICalculationUnit1D) m_parentCalcUnit).getInterpolationCount();

    if( m_parentCalcUnit instanceof ICalculationUnit1D2D )
      m_selectedSubUnits.addAll( ((ICalculationUnit1D2D) calcUnit).getSubCalculationUnits() );

    setTitle( m_parentCalcUnit.getName() );
    setDescription( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CalculationUnitPropertyWizardPage.1") ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    /* Find all 1d or 2d sub units which are not already a sub unit of the parent. */
    final List<ICalculationUnit> selectedSubUnits = m_selectedSubUnits;
    final List<ICalculationUnit> availableSubUnits = new ArrayList<>();
    for( final ICalculationUnit cUnit : m_availableCalcUnits )
    {
      if( cUnit instanceof ICalculationUnit1D || cUnit instanceof ICalculationUnit2D )
        availableSubUnits.add( cUnit );
    }
    availableSubUnits.removeAll( selectedSubUnits );

    /* Create the UI */
    final Composite rootPanel = new Composite( parent, SWT.NONE );
    rootPanel.setLayout( new GridLayout() );

    final Composite propertiesComp = createPropertiesPanel( rootPanel );
    propertiesComp.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );
    final Composite subUnitsComp = createSubUnitsPanel( rootPanel, selectedSubUnits, availableSubUnits );
    subUnitsComp.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    setControl( rootPanel );
  }

  private Composite createPropertiesPanel( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 2, false ) );

    final Label nameLabel = new Label( composite, SWT.NONE );
    nameLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    nameLabel.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CalculationUnitPropertyWizardPage.2") ); //$NON-NLS-1$
    final Text nameText = new Text( composite, SWT.BORDER );
    nameText.setText( m_name );
    nameText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    nameText.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        m_name = nameText.getText();
      }
    } );

    if( m_parentCalcUnit instanceof ICalculationUnit1D )
    {
      final Color goodColor = composite.getDisplay().getSystemColor( SWT.COLOR_BLACK );
      final Color badColor = composite.getDisplay().getSystemColor( SWT.COLOR_RED );

      final Label interpolLabel = new Label( composite, SWT.NONE );
      interpolLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
      interpolLabel.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CalculationUnitPropertyWizardPage.3") ); //$NON-NLS-1$
      final Text interpolText = new Text( composite, SWT.BORDER );
      interpolText.setText( "" + m_interpolationCount ); //$NON-NLS-1$
      interpolText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      interpolText.addModifyListener( new DoubleModifyListener( goodColor, badColor ) );
      interpolText.addModifyListener( new ModifyListener()
      {
        @Override
        public void modifyText( final ModifyEvent e )
        {
          final String interpol = interpolText.getText();

          try
          {
            m_interpolationCount = Integer.valueOf( interpol );
            setErrorMessage( null );
            setPageComplete( true );
          }
          catch( final NumberFormatException e1 )
          {
            setErrorMessage( e1.toString() );
            setPageComplete( false );
          }
        }
      } );
    }

    return composite;
  }

  private Composite createSubUnitsPanel( final Composite parent, final List<ICalculationUnit> selectedSubUnits, final List<ICalculationUnit> availableSubUnits )
  {
    final Composite composite = new Composite( parent, SWT.NULL );
    final FormLayout layout = new FormLayout();
    composite.setLayout( layout );

    if( !(m_parentCalcUnit instanceof ICalculationUnit1D2D) )
      return composite;

    FormData formData;

    // formData.left = new FormAttachment( 0, 5 );
    // formData.top = new FormAttachment( 0, 5 );

    // final Label name1D2D = new Label( composite, SWT.NONE );
    // name1D2D.setText( Messages.getString( "CreateSubCalculationUnitCopyWizardPage.2" ) + " " +
    // m_parentCalcUnit.getName() ); //$NON-NLS-1$ //$NON-NLS-2$
    // name1D2D.setLayoutData( formData );

    formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 15 );
    // formData.top = new FormAttachment( name1D2D, 15 );
    formData.height = 150;
    formData.right = new FormAttachment( 45, 0 );

    final Group availableCalcUnitsGroup = new Group( composite, SWT.NONE );
    availableCalcUnitsGroup.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CalculationUnitPropertyWizardPage.5") ) ; //$NON-NLS-1$
    availableCalcUnitsGroup.setLayoutData( formData );
    availableCalcUnitsGroup.setLayout( new GridLayout( 1, false ) );

    final TableViewer availableUnitsViewer = new TableViewer( availableCalcUnitsGroup );
    availableUnitsViewer.setContentProvider( new ArrayContentProvider() );
    availableUnitsViewer.setLabelProvider( new CalculationUnit1D2DLabelProvider() );

    availableUnitsViewer.setInput( availableSubUnits );

    {
      final Table calcUnitsTable = availableUnitsViewer.getTable();
      calcUnitsTable.setLinesVisible( true );
      calcUnitsTable.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    }

    final Button addButton = new Button( composite, SWT.PUSH );
    final Image addImage = new Image( composite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/forward.gif" ).getImageData() );  //$NON-NLS-1$
    addButton.setImage( addImage );

    formData = new FormData();
    formData.left = new FormAttachment( availableCalcUnitsGroup, 5 );
    formData.top = new FormAttachment( 45 );
    // formData.right = new FormAttachment(50,0);
    addButton.setLayoutData( formData );

    final Button removeButton = new Button( composite, SWT.PUSH );
    final Image removeImage = new Image( composite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/backward.gif" ).getImageData() );  //$NON-NLS-1$
    removeButton.setImage( removeImage );
    formData = new FormData();
    formData.left = new FormAttachment( availableCalcUnitsGroup, 5 );
    formData.top = new FormAttachment( addButton, 10 );
    removeButton.setLayoutData( formData );

    formData = new FormData();
    formData.left = new FormAttachment( removeButton, 5 );
    formData.top = new FormAttachment( 0, 15 );
    // formData.top = new FormAttachment( name1D2D, 15 );
    formData.height = 150;
    formData.right = new FormAttachment( 100, -5 );

    final Group selectedSubUnitsGroup = new Group( composite, SWT.NONE );
    selectedSubUnitsGroup.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CalculationUnitPropertyWizardPage.8") );  //$NON-NLS-1$
    selectedSubUnitsGroup.setLayoutData( formData );
    selectedSubUnitsGroup.setLayout( new GridLayout( 1, false ) );

    final TableViewer subUnitsViewer = new TableViewer( selectedSubUnitsGroup );
    subUnitsViewer.setContentProvider( new ArrayContentProvider() );
    subUnitsViewer.setLabelProvider( new CalculationUnit1D2DLabelProvider() );
    subUnitsViewer.setInput( selectedSubUnits );

    {
      final Table subCalcUnitsTable = subUnitsViewer.getTable();
      subCalcUnitsTable.setLinesVisible( true );
      subCalcUnitsTable.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    }

    /* Hook Listeners */
    addButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final ICalculationUnit selectedUnit = (ICalculationUnit) ((IStructuredSelection) availableUnitsViewer.getSelection()).getFirstElement();
        if( selectedUnit != null )
        {
          selectedSubUnits.add( selectedUnit );
          availableSubUnits.remove( selectedUnit );
          subUnitsViewer.refresh();
          availableUnitsViewer.refresh();
        }
      }

    } );

    removeButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final ICalculationUnit selectedUnit = (ICalculationUnit) ((IStructuredSelection) subUnitsViewer.getSelection()).getFirstElement();
        if( selectedUnit != null )
        {
          availableSubUnits.add( selectedUnit );
          selectedSubUnits.remove( selectedUnit );
          availableUnitsViewer.refresh();
          subUnitsViewer.refresh();
        }
      }
    } );

    return composite;
  }

  public ICalculationUnit[] getSelectedSubUnits( )
  {
    return m_selectedSubUnits.toArray( new ICalculationUnit[m_selectedSubUnits.size()] );
  }

  public String getChangedName( )
  {
    return m_name;
  }

  public int getChangedInterpolationCount( )
  {
    return m_interpolationCount;
  }

}
