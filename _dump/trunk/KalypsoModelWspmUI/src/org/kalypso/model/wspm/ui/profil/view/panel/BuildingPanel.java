package org.kalypso.model.wspm.ui.profil.view.panel;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.swt.events.DoubleModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilBuilding;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.BuildingEdit;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.profil.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;


/**
 * @author belger
 */
/**
 * @author kimwerner
 * 
 */
public class BuildingPanel extends AbstractProfilView
{
  private final Collection<Text> m_texts = new ArrayList<Text>( 8 );

  private final IProfilBuilding m_building;

  private String getLabel( final BUILDING_PROPERTY property )
  {
    if( property == BUILDING_PROPERTY.BREITE )
    {
      switch( m_building.getTyp() )
      {
        case EI:
          return "gr�sster Durchmesser [m]";
        case TRAPEZ:
          return "lange Seite [m]";
        case BRUECKE:
          return "Breite in Flie�richtung [m]";
        default:
          return property.toString();
      }
    }
    return property.toString();

  }

  public BuildingPanel( final IProfilEventManager pem, final ProfilViewData viewdata )
  {
    super( pem, viewdata, null );

    m_building = getProfil().getBuilding();
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite, int)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final int style )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout( 2, false );
    gridLayout.verticalSpacing = 15;
    panel.setLayout( gridLayout );
    panel.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final Display display = parent.getDisplay();
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

    for( final BUILDING_PROPERTY buildingProperty : m_building.getBuildingProperties() )
    {
      final String tooltip = buildingProperty.getTooltip();

      final Label label = new Label( panel, SWT.NONE );
      label.setLayoutData( new GridData( GridData.BEGINNING, GridData.BEGINNING, true, false ) );
      label.setText( getLabel( buildingProperty ) );
      label.setToolTipText( tooltip );

      final Text bldText = new Text( panel, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );
      m_texts.add( bldText );
      bldText.setLayoutData( new GridData( SWT.FILL, SWT.BEGINNING, true, false ) );

      bldText.addModifyListener( doubleModifyListener );
      bldText.setData( buildingProperty );
      bldText.setToolTipText( tooltip );
      bldText.addFocusListener( new FocusAdapter()
      {
        @Override
        public void focusGained( FocusEvent e )
        {
          bldText.selectAll();
        }

        /**
         * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
         */
        @Override
        public void focusLost( final FocusEvent e )
        {
          final double value = NumberUtils.parseQuietDouble( bldText.getText() );
          final IProfil profil = getProfil();
          final IProfilBuilding building = profil.getBuilding();
          if( Double.isNaN( value ) || (building == null) )
          {
            updateControls();
            return;
          }
          try
          {
            final double currentValue = (Double) building.getValueFor( buildingProperty );
            if( value == currentValue )
              return;
            final BuildingEdit edit = new BuildingEdit( profil.getBuilding(), buildingProperty, value );
            final ProfilOperation operation = new ProfilOperation( buildingProperty.getTooltip()+ " �ndern", getProfilEventManager(), edit, true );
            new ProfilOperationJob( operation ).schedule();
          }
          catch( ProfilDataException e1 )
          {
            updateControls();
          }
        }
      } );
    }

    updateControls();

    return panel;
  }

  protected void updateControls( )
  {
    for( final Text text : m_texts )
    {
      try
      {
        if( !text.isDisposed() )
        {
          text.setText( String.format( "%.2f", m_building.getValueFor( (BUILDING_PROPERTY) text.getData() ) ) );
          if( text.isFocusControl() )
            text.selectAll();
        }
      }
      catch( final ProfilDataException e )
      {
        text.setText("Formatfehler");
      }
    }
  }

  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    if( hint.isBuildingChanged() || hint.isBuildingDataChanged() )
    {
    final Control control = getControl();
    if( control != null && !control.isDisposed() )
      control.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          updateControls();
        }
      } );
    }
  }
}
