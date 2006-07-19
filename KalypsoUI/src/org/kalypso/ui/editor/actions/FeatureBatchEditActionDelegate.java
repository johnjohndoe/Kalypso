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
package org.kalypso.ui.editor.actions;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.AnnotationUtilities;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.command.RelativeFeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Action delegate class for feature batch editing
 * 
 * @author Stefan Kurzbach
 */
public class FeatureBatchEditActionDelegate implements IActionDelegate
{
  /**
   * Input dialog for selecting the action to take
   */
  private final class BatchEditParametersInputDialog extends InputDialog
  {

    private final class ButtonSelectionListener extends SelectionAdapter
    {
      ButtonSelectionListener( )
      {
      }

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        super.widgetSelected( e );
        final Button button = (Button) e.widget;
        if( button.getSelection() )
        {
          m_op = button.getText();
        }
      }
    }

    String m_op = "+";

    /**
     * Creates a new input dialog
     */
    public BatchEditParametersInputDialog( final Shell shell )
    {
      super( shell, "Folgende Operation auf alle Werte in der Auswahl anwenden", null, "0", null );
    }

    @Override
    protected Control createDialogArea( final Composite parent )
    {
      final Composite composite = (Composite) super.createDialogArea( parent );
      final Group m_radioButtonGroup = new Group( composite, SWT.SHADOW_ETCHED_IN );
      m_radioButtonGroup.setText( "Operation w‰hlen" );
      final FillLayout fillLayout = new FillLayout();
      fillLayout.type = SWT.VERTICAL;
      m_radioButtonGroup.setLayout( fillLayout );

      final ButtonSelectionListener buttonSelectionListener = new ButtonSelectionListener();

      final Button equalsButton = new Button( m_radioButtonGroup, SWT.RADIO );
      equalsButton.setToolTipText( "setzt den eingegebenen Wert" );
      equalsButton.setText( "=" );
      equalsButton.addSelectionListener( buttonSelectionListener );

      final Button plusButton = new Button( m_radioButtonGroup, SWT.RADIO );
      plusButton.setText( "+" );
      plusButton.setToolTipText( "addiert den eingegebenen Wert" );
      plusButton.addSelectionListener( buttonSelectionListener );
      plusButton.setSelection( true );

      final Button minusButton = new Button( m_radioButtonGroup, SWT.RADIO );
      minusButton.setText( "-" );
      minusButton.setToolTipText( "subtrahiert den eingegebenen Wert" );
      minusButton.addSelectionListener( buttonSelectionListener );

      final Button timesButton = new Button( m_radioButtonGroup, SWT.RADIO );
      timesButton.setText( "*" );
      timesButton.setToolTipText( "multipliziert mit dem eingegebenen Wert" );
      timesButton.addSelectionListener( buttonSelectionListener );

      final Button divideButton = new Button( m_radioButtonGroup, SWT.RADIO );
      divideButton.setToolTipText( "dividiert durch den eingegebenen Wert" );
      divideButton.setText( "/" );
      divideButton.addSelectionListener( buttonSelectionListener );

      return composite;
    }

    public String getOperator( )
    {
      return m_op;
    }

    public double getAmount( )
    {
      return Double.parseDouble( getValue() );
    }
  }

  private IPropertyType m_focusedProperty;

  private Feature[] m_selectedFeatures;

  private CommandableWorkspace m_workspace;

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final Shell shell = Display.getCurrent().getActiveShell();
    final BatchEditParametersInputDialog dialog = new BatchEditParametersInputDialog( shell );
    dialog.open();
    final String op = dialog.getOperator();
    final FeatureChange[] changeArray = new FeatureChange[m_selectedFeatures.length];
    for( int i = 0; i < m_selectedFeatures.length; i++ )
    {
      changeArray[i] = new RelativeFeatureChange( m_selectedFeatures[i], (IValuePropertyType) m_focusedProperty, op, dialog.getAmount() );
    }

    final ChangeFeaturesCommand changeFeaturesCommand = new ChangeFeaturesCommand( m_workspace, changeArray );

    try
    {
      m_workspace.postCommand( changeFeaturesCommand );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, "", e );
      ErrorDialog.openError( shell, action.getText(), changeFeaturesCommand.getDescription(), status );
    }

  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    if( selection instanceof IFeatureSelection )
    {
      final IFeatureSelection featureSelection = (IFeatureSelection) selection;
      m_focusedProperty = featureSelection.getFocusedProperty();
      addFocusDescription( action, m_focusedProperty );
      if( RelativeFeatureChange.isNumeric( m_focusedProperty ) )
      {
        action.setEnabled( true );
        m_selectedFeatures = FeatureSelectionHelper.getFeatures( featureSelection );
        final Feature focusedFeature = featureSelection.getFocusedFeature();
        m_workspace = featureSelection.getWorkspace( focusedFeature );
      }
      else
      {
        action.setEnabled( false );
      }
    }
  }

  private void addFocusDescription( final IAction action, final IPropertyType focusedProperty )
  {
    final String text = action.getText();
    if( text != null )
    {
      final String newText = text.replaceAll( " \\(.*\\)", "" ) + " (" + AnnotationUtilities.getAnnotation( focusedProperty ).getLabel() + ")";
      action.setText( newText );
    }
  }

}
