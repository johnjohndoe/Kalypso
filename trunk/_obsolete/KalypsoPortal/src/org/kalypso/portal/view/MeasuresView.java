package org.kalypso.portal.view;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.portal.ImageProvider;

public class MeasuresView extends ViewPart
{

  private Composite m_top;

  public MeasuresView( )
  {
  }

  @Override
  public void createPartControl( Composite parent )
  {
    m_top = new Composite( parent, SWT.NONE );
    m_top.setLayout( new GridLayout( 1, false ) );
    GridData data = new GridData();
    m_top.setLayoutData( data );
    final Group measuresGroup = new Group( m_top, SWT.NULL );
    measuresGroup.setText( "Mögliche Wasserwirtschaftliche Maßnahmen" );
    measuresGroup.setLayout( new GridLayout( 2, true ) );
    measuresGroup.setLayoutData( new GridData() );
    final Label rhbLabel = new Label( measuresGroup, SWT.NONE );
    rhbLabel.setText( "Regenrückhaltebecken:" );
    final Button measureRHB = new Button( measuresGroup, SWT.PUSH | SWT.FLAT );
    measureRHB.setImage( ImageProvider.IMAGE_MEASURE_RETENSION_BASIN.createImage() );
    measureRHB.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        MessageDialog.openInformation( getSite().getShell(), "FLOWS DSS Infodialog", "Hier wird im Measures.gml "
            + "das RHB-Element gewählt. Der User zeichnet ein Polygon und wählt einen Entwässerungspunkt aus der Karte." );
      }
    } );
    final Label mrsLabel = new Label( measuresGroup, SWT.NONE );
    mrsLabel.setText( "Mulden-Rigolen-System:" );
    Button measureMRS = new Button( measuresGroup, SWT.PUSH | SWT.FLAT );
    measureMRS.setImage( ImageProvider.IMAGE_MEASURE_SWALT_TRENCH.createImage() );
    measureMRS.addSelectionListener( new SelectionAdapter()
    {

      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        MessageDialog.openInformation( getSite().getShell(), "FLOWS DSS Infodialog", "Hier wird im Measures.gml "
            + "das MRS-Element gewählt. Der User zeichnet eine Linie im Plangebiet und wählt einen Entwässerungspunkt aus der Karte." );
      }
    } );
    final Label sealing = new Label( measuresGroup, SWT.NONE );
    sealing.setText( "Entsiegelung:" );
    Button desealingMeasure = new Button( measuresGroup, SWT.PUSH | SWT.FLAT );
    desealingMeasure.setImage( ImageProvider.IMAGE_MESASURE_CHANGE_SEALING.createImage() );
    desealingMeasure.addSelectionListener( new SelectionAdapter(){
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        MessageDialog.openInformation( getSite().getShell(), "FLOWS DSS Infodialog", "Hier wird im Measures.gml "
            + "das Landnutzungs-Element gewählt. Der User zeichnet ein Polygon und bestimmt den neuen Versieglungsgrad." );
      }
      
    });
    measuresGroup.pack();
  }

  @Override
  public void setFocus( )
  {

  }

}
