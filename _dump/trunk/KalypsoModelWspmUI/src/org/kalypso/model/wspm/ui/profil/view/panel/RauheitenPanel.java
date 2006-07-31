package org.kalypso.model.wspm.ui.profil.view.panel;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
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
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_PROPERTY;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.PARAMETER;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.DeviderEdit;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyHide;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.profil.view.AbstractProfilView;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;

/**
 * @author gernot
 */
public class RauheitenPanel extends AbstractProfilView
{
  Text m_VL;

  Text m_HF;

  Text m_VR;

  Button m_blockRauheit;

  boolean m_enablePanel;

  public RauheitenPanel( final IProfilEventManager pem, final ProfilViewData viewdata )
  {
    super( pem, viewdata, null );
  }

  /**
   * @see com.bce.profil.ui.view.AbstractProfilView#doCreateControl(org.eclipse.swt.widgets.Composite, int)
   */
  @Override
  protected Control doCreateControl( final Composite parent, final int style )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    final GridLayout gridLayout = new GridLayout( 3, true );
    panel.setLayout( gridLayout );
    panel.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    final IProfilBuilding building = getProfil().getBuilding();
    final POINT_PROPERTY[] pointProperties = (building == null) ? null : building.getPointProperties();
    // (building == null) ? true : (.length == 0)
    m_enablePanel = !((building != null) && (pointProperties == null));
    m_blockRauheit = new Button( panel, SWT.CHECK );
    m_blockRauheit.setSelection( !(Boolean) POINT_PROPERTY.RAUHEIT.getParameter( PARAMETER.VISIBLE ) );
    final IProfilDevider[] devs = (getProfil().getDevider( new DEVIDER_TYP[] { DEVIDER_TYP.DURCHSTROEMTE, DEVIDER_TYP.TRENNFLAECHE } ));
    m_blockRauheit.setEnabled( m_enablePanel && (devs != null) );
    m_blockRauheit.setText( "einfache Rauheiten verwenden" );
    final GridData blockData = new GridData();
    blockData.horizontalSpan = 3;

    m_blockRauheit.setLayoutData( blockData );
    m_blockRauheit.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( org.eclipse.swt.events.SelectionEvent e )
      {

        final boolean isBlockRauheit = !(Boolean) POINT_PROPERTY.RAUHEIT.getParameter( PARAMETER.VISIBLE );
        if( isBlockRauheit == m_blockRauheit.getSelection() )
          return;

        final IProfilChange change = new PointPropertyHide( POINT_PROPERTY.RAUHEIT, isBlockRauheit );
        final ProfilOperation operation = new ProfilOperation( "Rauheiten Blockweise setzen", getProfilEventManager(), change, true );
        new ProfilOperationJob( operation ).schedule();
      }
    } );
    addLabel( panel, "VL", "Vorland links" );
    addLabel( panel, "HF", "Hauptöffnung" );
    addLabel( panel, "VR", "Vorland rechts" );

    m_VL = addText( panel );
    m_HF = addText( panel );
    m_VR = addText( panel );
    updateControls();
    return panel;
  }

  private Text addText( Composite panel )
  {
    final Display display = panel.getDisplay();
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final DoubleModifyListener doubleModifyListener = new DoubleModifyListener( goodColor, badColor );

    final IProfil profil = getProfil();
    final Text t = new Text( panel, SWT.TRAIL | SWT.SINGLE | SWT.BORDER );

    final GridData data = new GridData();
    data.grabExcessHorizontalSpace = true;
    data.horizontalAlignment = GridData.FILL;
    t.setLayoutData( data );

    t.setEnabled( (Boolean) POINT_PROPERTY.RAUHEIT.getParameter( PARAMETER.VISIBLE ) );
    t.addModifyListener( doubleModifyListener );
    t.addFocusListener( new FocusAdapter()
    {
      @Override
      public void focusGained( FocusEvent e )
      {
        ((Text) e.widget).selectAll();
      }

      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusLost( final FocusEvent e )
      {
        if( m_VL.isDisposed() || m_HF.isDisposed() || m_VR.isDisposed() )
          return;
        final IProfilDevider[] deviders = profil.getDevider( new DEVIDER_TYP[] { DEVIDER_TYP.DURCHSTROEMTE, DEVIDER_TYP.TRENNFLAECHE } );

        final Double value1 = NumberUtils.parseQuietDouble( m_VL.getText() );
        final Double value2 = NumberUtils.parseQuietDouble( m_HF.getText() );
        final Double value3 = NumberUtils.parseQuietDouble( m_VR.getText() );
        final Double oldvalue1 = (Double) deviders[0].getValueFor( DEVIDER_PROPERTY.RAUHEIT );
        final Double oldvalue2 = (Double) deviders[1].getValueFor( DEVIDER_PROPERTY.RAUHEIT );
        final Double oldvalue3 = (Double) deviders[2].getValueFor( DEVIDER_PROPERTY.RAUHEIT );
        final ArrayList<IProfilChange> changes = new ArrayList<IProfilChange>();
        final Double prec = (Double) POINT_PROPERTY.RAUHEIT.getParameter( PARAMETER.PRECISION );
        if( Math.abs( oldvalue1 - value1 ) > prec )
        {
          changes.add( new DeviderEdit( deviders[0], DEVIDER_PROPERTY.RAUHEIT, value1.isNaN() ? oldvalue1 : value1 ) );
        }
        if( Math.abs( oldvalue2 - value2 ) > prec )
        {
          changes.add( new DeviderEdit( deviders[1], DEVIDER_PROPERTY.RAUHEIT, value2.isNaN() ? oldvalue2 : value2 ) );
        }
        if( Math.abs( oldvalue3 - value3 ) > prec )
        {
          changes.add( new DeviderEdit( deviders[2], DEVIDER_PROPERTY.RAUHEIT, value3.isNaN() ? oldvalue3 : value3 ) );
        }
        final ProfilOperation operation = new ProfilOperation( "Rauheiten Blockweise setzen", getProfilEventManager(), changes.toArray( new IProfilChange[0] ), true );
        new ProfilOperationJob( operation ).schedule();
      }
    } );

    return t;
  }

  private void addLabel( final Composite parent, final String text, final String toolTip )
  {
    final Label label = new Label( parent, SWT.CENTER );
    label.setText( text );
    label.setToolTipText( toolTip );
    final GridData data = new GridData();
    data.grabExcessHorizontalSpace = true;
    data.horizontalAlignment = GridData.CENTER;
    label.setLayoutData( data );
  }

  @SuppressWarnings("boxing")
  void updateControls( )
  {
    final IProfilDevider[] devider = getProfil().getDevider( new DEVIDER_TYP[] { DEVIDER_TYP.DURCHSTROEMTE, DEVIDER_TYP.TRENNFLAECHE } );
    Boolean isBlockSetting = !(Boolean) POINT_PROPERTY.RAUHEIT.getParameter( PARAMETER.VISIBLE );
    if( !m_blockRauheit.isDisposed() )
    {
      m_blockRauheit.setSelection( isBlockSetting );
    }
    if( !m_VL.isDisposed() )
    {
      m_VL.setEnabled( isBlockSetting && m_enablePanel );
      if( (devider != null) && (devider.length > 0) )
        setBlockValue( m_VL, devider[0] );

    }
    if( !m_HF.isDisposed() )
    {
      m_HF.setEnabled( isBlockSetting && m_enablePanel );
      if( (devider != null) && (devider.length > 1) )
        setBlockValue( m_HF, devider[1] );
    }

    if( !m_VR.isDisposed() )
    {
      m_VR.setEnabled( isBlockSetting && m_enablePanel );
      if( (devider != null) && (devider.length > 2) )
        setBlockValue( m_VR, devider[2] );
    }

  }

  private void setBlockValue( final Text text, final IProfilDevider devider )
  {
    final Double value = (Double) devider.getValueFor( DEVIDER_PROPERTY.RAUHEIT );
    try
    {
      text.setText( String.format( "%.2f", (value == null) ? devider.getPoint().getValueFor( POINT_PROPERTY.RAUHEIT ) : value ) );
    }
    catch( ProfilDataException e )
    {
      text.setText( "error" );
    }
    if( text.isFocusControl() )
      text.selectAll();
  }

  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
    if( hint.isPointPropertiesChanged() )
    {
      final Control control = getControl();
      if( control != null && !control.isDisposed() )
      {
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
}
