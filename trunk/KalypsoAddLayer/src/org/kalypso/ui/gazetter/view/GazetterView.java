package org.kalypso.ui.gazetter.view;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapPanelProvider;
import org.kalypso.view.gazetter.GazetterLocationType;
import org.kalypso.view.gazetter.ObjectFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;

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

  private final HashMap<GazetterLocationType, GazetteerControl> m_GazetteerLocation2Control = new HashMap<GazetterLocationType, GazetteerControl>();

  private final HashMap<GazetteerControl, GazetterLocationType> m_GazetteerControl2Location = new HashMap<GazetteerControl, GazetterLocationType>();

  public GazetterView( )
  {
    // do nothing
  }

  @Override
  public void createPartControl( Composite parent )
  {
    final FormToolkit toolkit = new FormToolkit( parent.getDisplay() );
    // prepare top composite
    final Composite baseComposite = toolkit.createComposite( parent, SWT.FLAT | SWT.TOP );
    baseComposite.setLayout( new GridLayout( 1, false ) );

    // load gazetteer configuration
    final org.kalypso.view.gazetter.GazetterView gView = getGazetterView();
    final List<GazetterLocationType> gazetterLocation = gView.getGazetterLocation();
    final URL baseURL;
    try
    {
      baseURL = new URL( gView.getBaseURL() );
      createComposite( baseComposite, gazetterLocation, toolkit, baseURL );
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }
    toolkit.paintBordersFor( baseComposite );
    initLists( gazetterLocation );
  }

  private void initLists( List<GazetterLocationType> gazetterLocation )
  {
    final Iterator<GazetterLocationType> iterator = gazetterLocation.iterator();
    while( iterator.hasNext() )
    {
      final GazetterLocationType gz = iterator.next();
      final GazetteerControl control = m_GazetteerLocation2Control.get( gz );
      try
      {
        control.init( null, null, null );
      }
      catch( Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
  }

  private void createComposite( Composite parent, List<GazetterLocationType> gLocations, FormToolkit toolkit, URL baseURL )
  {
    final Iterator<GazetterLocationType> iterator = gLocations.iterator();
    while( iterator.hasNext() )
    {
      final GazetterLocationType gLocation = iterator.next();
      final GazetteerControl gControl = new GazetteerControl( gLocation, baseURL, this );
      register( gLocation, gControl );

      final Composite base = toolkit.createComposite( parent, SWT.FLAT | SWT.TOP );
      int columns = 1;
      if( gLocation.isDoTextSearch() )
        columns++;

      final List<GazetterLocationType> childs = gLocation.getGazetterLocation();
      if( !childs.isEmpty() )
        columns++;
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
        text.addKeyListener( new KeyAdapter()
        {

          @Override
          public void keyReleased( KeyEvent e )
          {
            if( e.keyCode == SWT.CR )
            {
              final String queryText = text.getText();
              gControl.init( null, null, queryText );
            }
          }

        } );
        text.addFocusListener( new FocusAdapter()
        {
          @Override
          public void focusLost( FocusEvent e )
          {
            final String queryText = text.getText();
            gControl.init( null, null, queryText );
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
      combo.getControl().setLayoutData( new GridData( GridData.CENTER | GridData.GRAB_HORIZONTAL | GridData.FILL_HORIZONTAL ) );
      combo.addSelectionChangedListener( gControl );

      combo.setLabelProvider( new GazetteerLabelProvider( gLocation.getLabelProperty() ) );
      final Button button = toolkit.createButton( comboBase, "GOTO", SWT.NONE );
      button.setLayoutData( new GridData( GridData.CENTER ) );
      button.addSelectionListener( new SelectionAdapter()
      {

        @Override
        public void widgetSelected( SelectionEvent e )
        {
          final IViewSite viewSite = getViewSite();
          final IEditorPart activeEditor = viewSite.getPage().getActiveEditor();
          if( activeEditor instanceof IMapPanelProvider && activeEditor instanceof ICommandTarget )
          {
            final IStructuredSelection selection = (IStructuredSelection) combo.getSelection();
            Object firstElement = selection.getFirstElement();
            if( firstElement instanceof Feature )
            {
              final Feature feature = (Feature) firstElement;
              final QName geographicExtentProp = gLocation.getGeographicExtentProperty();
              final Object property = feature.getProperty( geographicExtentProp );
              if( property instanceof GM_Point )
              {
                // TODO
                System.out.println( "TODO" );
              }
              else if( property instanceof GM_Object)
              {
                final GM_Object geom = (GM_Object) property;
                // GM_Point centroid = geom.getCentroid();
                final MapPanel mapPanel = ((IMapPanelProvider) activeEditor).getMapPanel();
                final ChangeExtentCommand command = new ChangeExtentCommand( mapPanel, geom.getEnvelope() );
                ((ICommandTarget) activeEditor).postCommand( command, null );
              }
              else
                MessageDialog.openInformation( getSite().getShell(), "Gazetteer-Info", "Gazetter lierferte keinen Raumbezug" );

            }
            // mapEditor.postCommand(command, runnable);

          }
          else
            MessageDialog.openInformation( getSite().getShell(), "Gazetteer-Info", "Die Aktion kann keiner Karte zugeordnet werden, bitte aktivieren Sie vorher eine Kartenansicht" );
          // get active map:
        }

      } );

      gControl.setViewer( combo, button );
      combo.setInput( new String[] { "...auswaehlen", "test" } );
      // childs
      if( !childs.isEmpty() )
      {
        final Composite childBase = toolkit.createComposite( base, SWT.FLAT | SWT.TOP );
        // childBase.setLayoutData( new GridData( GridData.FILL_BOTH | GridData.VERTICAL_ALIGN_BEGINNING |
        // GridData.GRAB_VERTICAL ) );
        childBase.setLayoutData( new GridData( GridData.VERTICAL_ALIGN_BEGINNING ) );
        childBase.setLayout( new GridLayout( 1, false ) );
        createComposite( childBase, childs, toolkit, baseURL );
      }
    }
  }

  private void register( GazetterLocationType location, GazetteerControl control )
  {
    m_GazetteerLocation2Control.put( location, control );
    m_GazetteerControl2Location.put( control, location );
  }

  private org.kalypso.view.gazetter.GazetterView getGazetterView( )
  {
//    final URL resource = getClass().getResource( "resources/gazetteerView.xml" );
    final URL resource = getClass().getResource( "resources/gazetteerViewFLOWS.xml" );
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

  public GazetteerControl getGControlForGLocation( GazetterLocationType location )
  {
    return m_GazetteerLocation2Control.get( location );
  }

}
