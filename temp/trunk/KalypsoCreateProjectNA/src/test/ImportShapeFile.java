package test;

import java.net.URL;
import java.util.HashMap;
import java.util.StringTokenizer;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypsodeegree.model.feature.Annotation;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuepfer
 *  
 */
public class ImportShapeFile
{

  GMLWorkspace shapeFile;

  GMLWorkspace modelFile;

  HashMap m_mapping = new HashMap();

//  private static Group m_sourceGroup;

  static String noElements[] =
  {
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "10",
      "11",
      "12",
      "13",
      "14",
      "15",
      "16",
      "17",
      "18",
      "19",
      "20",
      "21",
      "22",
      "23",
      "24",
      "25",
      "26",
      "27",
      "28",
      "29",
      "30",
      "31",
      "32",
      "33",
      "34",
      "34",
      "35",
      "36",
      "37",
      "38",
      "39",
      "40",
      "42",
      "43",
      "44",
      "45",
      "46",
      "47",
      "48",
      "49",
      "50",
      "51",
      "52",
      "53",
      "54",
      "55",
      "56",
      "57",
      "58",
      "59",
      "60" };

  public static void main( String[] args )
  {

    //String shpBase = "D://Daten//DataForCK//KalypsoQM//GIS//teilgebiete_qm";
    String shpBase = "D://KalypsoWorkspace//Spree//Grunddaten//Shapes//sachsen";
//    URL modelURL;
    URL schemaURL;

    CS_CoordinateSystem cs = ConvenienceCSFactory.getInstance().getOGCCSByName( "EPSG:31467" );
    try
    {

//      modelURL = ( new File( "D://temp//modell.gml" ) ).toURL();
      schemaURL = new URL( "file:/D:/KalypsoWorkspace/KalypsoWeisseElster/.model/schema/namodell.xsd" );
//      String schema = schemaURL.getPath();
//      String host = schemaURL.getHost();
      GMLWorkspace gmlWS = ShapeSerializer.deserialize( shpBase, cs );
      ShapeSerializer.serialize( gmlWS, "D://Temp//test" );
      Feature rootFeature = gmlWS.getRootFeature();
//      List propertyList = (List)rootFeature.getProperty( "featureMember" );

//      Feature feature = (Feature)propertyList.get( 0 );
//      Object o = feature.getProperty( "TEILGEBNR" );
//      if( o instanceof Integer )
//      {
//        int no = ( (Integer)o ).intValue();
//      }
      System.out.println( "test" );

      FeatureType rootFT = rootFeature.getFeatureType();

      FeatureAssociationTypeProperty ftp = (FeatureAssociationTypeProperty)rootFT.getProperty( "featureMember" );

      FeatureType[] associationFeatureTypes = ftp.getAssociationFeatureTypes();
      FeatureType shapeFT = associationFeatureTypes[0];
      FeatureTypeProperty[] sourceFtp = shapeFT.getProperties();

      //			FeatureTypeProperty o = shapeFT.getProperty("TEILGEBNR");

      final ITypeRegistry registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
      registry.registerTypeHandler( new ObservationLinkHandler() );
      GMLSchema gmlSchema = GMLSchemaCatalog.getSchema( schemaURL );
      FeatureType targetFT = gmlSchema.getFeatureType( "Node" );
      FeatureTypeProperty targetFTP = targetFT.getProperty( "num" );
      //      String key = Locale.getDefault().getLanguage();
      final String key = KalypsoGisPlugin.getDefault().getPluginPreferences().getString( IKalypsoPreferences.LANGUAGE );
      Annotation annotation = targetFTP.getAnnotation( key );
      String label = annotation.getLabel();
      String tooltip = annotation.getTooltip();
      System.out.println( label + "/t" + tooltip );

      //			GMLWorkspace gmlModel = GmlSerializer.createGMLWorkspace(modelURL,
      //					schemaURL);
      //			GMLWorkspace gmlModel = GmlSerializer.createGMLWorkspace(modelURL, new UrlResolver());

      FeatureType catchmentFt = gmlSchema.getFeatureType( "Catchment" );
      //			Featurefeature = FeatureFactory.createFeature("c1",
      // catchmentFt);
      //			FeatureProperty fpNum = FeatureFactory.createFeatureProperty(
      //					"inum", new Integer(100));
      //			feature.addProperty(fpNum);
      //			FeatureTypeProperty[] targetFtp = sourceFtp;
      FeatureTypeProperty[] targetFtp = catchmentFt.getProperties();

      //Control
      Display display = new Display();
      Shell shell = new Shell( display );

      GridLayout gridLayout = new GridLayout();
      gridLayout.numColumns = 1;
      shell.setLayout( gridLayout );
      //Top Group
      Composite group = new Composite( shell, SWT.NONE );
      group.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
      ScrolledComposite topSCLComposite = new ScrolledComposite( shell, SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER );
      final Composite topComposite = new Composite( topSCLComposite, SWT.NONE );
      //group.setSize(400,600);
      topSCLComposite.setContent( topComposite );
      topSCLComposite.setVisible( true );
      topComposite.setVisible( true );
      GridLayout topCompositeLayout = new GridLayout();
      topComposite.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
      topCompositeLayout.numColumns = 1;
      //topCompositeLayout.makeColumnsEqualWidth = true;
      topComposite.setLayout( topCompositeLayout );
      //topComposite.pack();

      Group topGroup = new Group( topComposite, SWT.NONE );
      topGroup.setText( "Mapping der Feature" );
      topGroup.setVisible( true );
      GridLayout topGroupLayout = new GridLayout();
      topGroupLayout.numColumns = 2;
      topGroup.setLayout( topGroupLayout );
      //topGroup.pack();

      //Source
      Group sourceGroup = new Group( topGroup, SWT.NONE );
      GridLayout sourceGroupLayout = new GridLayout();
      sourceGroupLayout.numColumns = 1;
      sourceGroupLayout.verticalSpacing = 2;
      sourceGroup.setLayout( sourceGroupLayout );
      sourceGroup.setVisible( true );
      sourceGroup.setText( "Quelle" );
      for( int j = 0; j < targetFtp.length; j++ )
      {
        Combo combo = new Combo( sourceGroup, SWT.READ_ONLY | SWT.DROP_DOWN | SWT.SINGLE );
        combo.setData( "target", targetFtp[j].getName() );
        combo.setLayoutData( new GridData( GridData.CENTER ) );
        combo.redraw();
        combo.addSelectionListener( new SelectionAdapter()
        {
          public void widgetSelected( SelectionEvent e )
          {
            Widget w = e.widget;
            StringTokenizer st = new StringTokenizer( w.toString() );
            st.nextToken();
            String str = st.nextToken();
            String name = str.substring( 1, str.length() - 1 );
            w.setData( "source", name );
            System.out.println( "Quelle: " + w.getData( "source" ) + "\tZiel: " + w.getData( "target" ) );
          }
        } );
        for( int i = 0; i < sourceFtp.length; i++ )
        {
          if( i == 0 )
            combo.add( "-NULL-" );
          combo.add( sourceFtp[i].getName() );
        }
        combo.select( 0 );
      }
      //sourceGroup.pack();
      //sourceGroup.redraw();
      //Target

      Group targetGroup = new Group( topGroup, SWT.NONE );
      GridLayout targetGroupLayout = new GridLayout();
      targetGroupLayout.numColumns = 1;
      targetGroupLayout.verticalSpacing = 10;
      targetGroup.setVisible( true );
      targetGroup.setLayout( targetGroupLayout );
      targetGroup.setText( "Ziel" );
      //			Table table0 = new Table (targetGroup, SWT.BORDER |
      // SWT.V_SCROLL);
      for( int i = 0; i < targetFtp.length; i++ )
      {
        FeatureTypeProperty featureTypeProperty = targetFtp[i];
        String name = featureTypeProperty.getName();
        //				table0.setLinesVisible (true);
        //				TableItem tableItem1 = new TableItem (table0, SWT.NONE);
        //				tableItem1.setText (name);
        Text text = new Text( targetGroup, SWT.NONE | SWT.READ_ONLY );
        text.setText( name );
      }

      Group buttonbar = new Group( shell, SWT.NONE );
      buttonbar.setText( "Zuordnung abschliessen" );
      GridLayout buttonbarLayout = new GridLayout();
      buttonbarLayout.numColumns = 2;
      buttonbar.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
      buttonbar.setLayout( buttonbarLayout );
      Button okButton = new Button( buttonbar, SWT.PUSH );
      okButton.setText( "Auswahl best�tigen" );
      okButton.addSelectionListener( new SelectionAdapter()
      {
        public void widgetSelected( SelectionEvent e )
        {
          handelOKSelection();
        }
      } );
      Button resetButton = new Button( buttonbar, SWT.PUSH );
      resetButton.setText( "Auswahl zur�cksetzen" );
      resetButton.addSelectionListener( new SelectionAdapter()
      {
        public void widgetSelected( SelectionEvent e )
        {
          handelResetSelection();
        }
      } );
      //			buttonbar.setVisible(true);
      //			sourceGroup.pack();
      //			targetGroup.pack();
      //			topGroup.pack();
      //			topSCLComposite.pack();
      //			Point pt = topComposite.computeSize(SWT.DEFAULT, SWT.DEFAULT);
      //	         topSCLComposite.setExpandHorizontal(true);
      //	         topSCLComposite.setExpandVertical(true);
      //	         topSCLComposite.setMinWidth(pt.x);
      //	         topSCLComposite.setMinHeight(pt.y);

      Point size = topComposite.computeSize( SWT.DEFAULT, SWT.DEFAULT );
      topComposite.setSize( size );
      GridData topCompoGridData = new GridData();
      topCompoGridData.widthHint = Math.min( size.x, topComposite.getSize().x );
      topCompoGridData.heightHint = Math.min( size.y, 200 );
      topSCLComposite.setLayoutData( topCompoGridData );
      topComposite.layout();
      topComposite.pack();

      shell.pack();
      shell.open();

      Control[] cArray = sourceGroup.getChildren();
      for( int i = 0; i < cArray.length; i++ )
      {
        Control c = cArray[i];
        System.out.println( c );
        Combo combo = (Combo)c;
        System.out.println( combo );
      }
      while( !shell.isDisposed() )
      {
//        m_sourceGroup = sourceGroup;
        if( !display.readAndDispatch() )
          display.sleep();
      }

      display.dispose();

      //			Group group = new Group(toplevelShell, 0);
      //			Combo noSelect = new Combo(toplevelShell,SWT.READ_ONLY |
      // SWT.DROP_DOWN | SWT.SINGLE);
      //			noSelect.add("- null -");
      //			for(int i = 0; i < ftp1.length; i++){
      //				Combo combo = new Combo(toplevelShell,SWT.READ_ONLY |
      // SWT.DROP_DOWN | SWT.SINGLE);
      //				FeatureTypeProperty featureTypeProperty = ftp1[i];
      //				String name = featureTypeProperty.getName();
      //				combo.add(name);
      //				combo.addSelectionListener(new SelectionAdapter() {
      //					public void widgetSelected(SelectionEvent e) {
      //						handleComboSelection(e);
      //					}
      //				});
      //				toplevelShell.redraw();
      //			}
      //			while (!toplevelShell.isDisposed ()) {
      //				if (!display.readAndDispatch ())
      //					display.sleep ();
      //			}
      //			display.dispose ();
      //			while (true){
      //				MessageBox box = new MessageBox(toplevelShell, SWT.RETRY |
      // SWT.CANCEL | SWT.APPLICATION_MODAL | SWT.ICON_QUESTION);
      //				box.setText("Test");
      //				box.setMessage("Nochmal ?");
      //				if(box.open() == SWT.CANCEL)break;
      //			}
      //			MappingControl mc = new MappingControl(toplevelShell,
      // toplevelShell.getStyle(), shapeFT, shapeFT);

      //List featureList = (List) rootFeature.getProperty("featureMember");

      //FeatureType riverFt = gmlSchema.getFeatureType("")

      //			String description = (String) feature.getProperty("description");

    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

//  protected static void handleComboSelection( String name, int j )
//  {
//
//  }

  protected static void handelOKSelection()
  {
    System.out.println( "OK wurde gedr�ckt" );
  }

  protected static void handelResetSelection()
  {
    System.out.println( "RESET wurde gedr�ckt" );
  }

//  public void test()
//  {
//    URL url = getClass().getResource( "lkjfds" );
//    //	  IFile.create...
//
//  }
}