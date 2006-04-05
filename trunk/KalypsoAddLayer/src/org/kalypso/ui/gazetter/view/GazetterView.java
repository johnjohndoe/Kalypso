package org.kalypso.ui.gazetter.view;

import java.net.URL;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.FormColors;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.view.gazetter.GazetterLocationType;
import org.kalypso.view.gazetter.ObjectFactory;

/**
 * <em>
 * 
 *  Bezirk                                  Strasse
 *  _________    ___________ _              ___________ _
 * |*abc_____|  |___________|V|GOTO        |___________|V|GOTO
 * 
 * 
 * 
 *  Label                 Strasse
 *  ___________ _          ___________ _
 * |___________|V|        |___________|V|
 * 
 * label
 * description
 * 
 * The title of the combobox is the <code>label</code>. The <code>description</code> acts as tooltip.
 * 
 * each combobox decorates a list of feature of the featuretype <code>featuretypeName</code>.
 * The spatial identifier (acts as ID) of the extend is the value of the property <code>spacialIdentifier</code>.  
 * The labeled items in the combobox are the values of the property <code>labelPropertyName</code>.
 * The geometry associated with is the value of the property <code>geographicExtendPropertyName</code>.
 * 
 * featuretype
 *  labelProperty
 *  spacialIdentifierProperty
 *  geographicExtendProperty
 * 
 * childs are the elements that can be queried for more detailed extents.
 * After selecting a parent, the direct childs may update their comboboxes. 
 * 
 * </em>
 * 
 * @author doemming
 */

public class GazetterView extends ViewPart
{

  public GazetterView( )
  {
    // do nothing
  }

  @Override
  public void createPartControl( Composite parent )
  {
    final FormToolkit toolkit = new FormToolkit( parent.getDisplay() );
    // prepare top composite
    final Composite base = toolkit.createComposite( parent, SWT.FLAT | SWT.TOP );
    base.setLayout( new GridLayout( 1, false ) );

    // load gazetteer configuration
    final org.kalypso.view.gazetter.GazetterView gView = getGazetterView();
    final List<GazetterLocationType> gazetterLocation = gView.getGazetterLocation();
    createComposite( base, gazetterLocation, toolkit );

    toolkit.paintBordersFor( base );
  }

  private void createComposite( Composite parent, List<GazetterLocationType> gLocations, FormToolkit toolkit )
  {
    final Iterator<GazetterLocationType> iterator = gLocations.iterator();
    while( iterator.hasNext() )
    {
      final GazetterLocationType gLocation = iterator.next();
      final GazetteerControl gControl = new GazetteerControl( gLocation, this );
      register( gControl );

      final Composite base = toolkit.createComposite( parent, SWT.FLAT | SWT.TOP );
      int columns = 1;
      if( gLocation.isDoTextSearch() )
        columns++;

      final List<GazetterLocationType> childs = gLocation.getGazetterLocation();
      if( !childs.isEmpty() )
        columns++;
      gControl.setEnableRunnable( new Runnable()
      {
        public void run( )
        {
          if( !base.isDisposed() )
            base.setEnabled( true );
        }
      } );
      gControl.setDisableRunnable( new Runnable()
      {
        public void run( )
        {
          if( !base.isDisposed() )
            base.setEnabled( false );
        }
      } );
      base.setLayout( new GridLayout( columns, false ) );
      // base.setLayoutData( new GridData( GridData.VERTICAL_ALIGN_BEGINNING | GridData.GRAB_VERTICAL ) );
      base.setLayoutData( new GridData( GridData.VERTICAL_ALIGN_BEGINNING ) );
      // search
      if( gLocation.isDoTextSearch() )
      {
        final Composite searchBase = toolkit.createComposite( base );
        searchBase.setLayoutData( new GridData( GridData.CENTER | GridData.GRAB_HORIZONTAL ) );
        searchBase.setLayout( new GridLayout( 1, false ) );
        final Label label = toolkit.createLabel( searchBase, "Recherche: " + gLocation.getLabel(), SWT.FLAT );
        label.setLayoutData( new GridData( GridData.BEGINNING ) );
        final Text text = toolkit.createText( searchBase, "...", SWT.BORDER );
        text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
        text.addFocusListener( new FocusAdapter()
        {
          @Override
          public void focusLost( FocusEvent e )
          {
            final String queryText = text.getText();
            gControl.query( queryText );
          }
        } );
      }

      // combo
      final Composite comboBase = toolkit.createComposite( base, SWT.TOP );
      comboBase.setLayoutData( new GridData( GridData.VERTICAL_ALIGN_BEGINNING | GridData.CENTER ) );
      comboBase.setLayout( new GridLayout( 2, false ) );
      final Label label = toolkit.createLabel( comboBase, gLocation.getLabel(), SWT.FLAT );
      final String toolTip = gLocation.getDescription();
      if( toolTip != null )
        label.setToolTipText( toolTip );
      final GridData gData = new GridData( GridData.BEGINNING );
      gData.horizontalSpan = 2;
      label.setLayoutData( gData );

      // final Combo combo = new Combo( comboBase, SWT.FLAT | SWT.READ_ONLY );
      final ComboViewer combo = new ComboViewer( comboBase, SWT.FLAT | SWT.READ_ONLY );
      combo.setData( FormToolkit.KEY_DRAW_BORDER, FormToolkit.TEXT_BORDER );
      combo.setContentProvider( gControl );
      combo.setInput( new String[] { "...auswaehlen", "test" } );
      combo.getControl().setLayoutData( new GridData( GridData.CENTER | GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
      combo.addSelectionChangedListener( gControl );
      final Button button = toolkit.createButton( comboBase, "GOTO", SWT.NONE );
      button.setLayoutData( new GridData( GridData.CENTER ) );
      button.addSelectionListener( gControl );
      gControl.setRefreshComboRunnable( new Runnable()
      {
        public void run( )
        {
          combo.refresh();
        }
      } );

      // childs
      if( !childs.isEmpty() )
      {
        final Composite childBase = toolkit.createComposite( base, SWT.FLAT | SWT.TOP );
        // childBase.setLayoutData( new GridData( GridData.FILL_BOTH | GridData.VERTICAL_ALIGN_BEGINNING |
        // GridData.GRAB_VERTICAL ) );
        childBase.setLayoutData( new GridData( GridData.VERTICAL_ALIGN_BEGINNING ) );
        childBase.setLayout( new GridLayout( 1, false ) );
        createComposite( childBase, childs, toolkit );
      }
    }
  }

  private void register( GazetteerControl control )
  {
    // TODO Auto-generated method stub

  }

  private org.kalypso.view.gazetter.GazetterView getGazetterView( )
  {
    final URL resource = getClass().getResource( "resources/gazetteerView.xml" );
    final JAXBContext context = JaxbUtilities.createQuiet( ObjectFactory.class );
    try
    {
      final Unmarshaller unmarshaller = context.createUnmarshaller();
      final Object object = unmarshaller.unmarshal( resource );
      return (org.kalypso.view.gazetter.GazetterView) object;
    }
    catch( JAXBException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return null;
    }
  }

  @Override
  public void setFocus( )
  {
    // TODO Auto-generated method stub

  }

}
