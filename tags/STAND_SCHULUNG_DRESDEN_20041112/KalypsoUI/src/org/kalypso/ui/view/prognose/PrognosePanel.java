package org.kalypso.ui.view.prognose;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.kalypso.eclipse.swt.graphics.FontUtilities;
import org.kalypso.model.xml.Modellist;
import org.kalypso.model.xml.ModellistType;
import org.kalypso.model.xml.ObjectFactory;
import org.kalypso.model.xml.ModellistType.ModelType;
import org.xml.sax.InputSource;

/**
 * @author belger
 */
public class PrognosePanel
{
  private Modellist m_modellist = null;

  private String m_errorMessage;

  private final Map m_imageHash = new HashMap();

  private final ModelLabelProvider m_labelProvider = new ModelLabelProvider();

  /** Alle Fonts in dieser Collection werden in der dispose Methode auch disposed */
  private final Collection m_disposeFonts = new LinkedList();

  private final URL m_location;

  private Label m_imageLabel;
  
  private Composite m_control;

  private ModelType m_model;

  public PrognosePanel( final URL modellistLocation )
  {
    m_location = modellistLocation;

    try
    {

      final InputSource inputSource = new InputSource( modellistLocation.openStream() );
      m_modellist = (Modellist)new ObjectFactory().createUnmarshaller().unmarshal( inputSource );

    }
    catch( final Exception e )
    {
      e.printStackTrace();

      m_errorMessage = e.getLocalizedMessage();
    }
  }

  public void dispose()
  {
    m_labelProvider.dispose();
    
    for( final Iterator iter = m_disposeFonts.iterator(); iter.hasNext(); )
      ((Font)iter.next()).dispose();
  }

  public Composite createControl( final Composite parent )
  {
    final Display display = parent.getDisplay();
    
    m_control = new Composite( parent, SWT.NONE );

    final GridLayout gridLayout = new GridLayout( 2, false );
    gridLayout.horizontalSpacing = 20;
    gridLayout.verticalSpacing = 20;
    gridLayout.marginHeight = 20;
    gridLayout.marginWidth = 20;
    m_control.setLayout( gridLayout );
    final GridData gridData = new GridData( GridData.FILL_BOTH );
    gridData.horizontalAlignment = GridData.CENTER;
    m_control.setLayoutData( gridData );

    if( m_modellist == null )
    {
      final Label label = new Label( parent, SWT.CENTER );
      label.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      label.setText( "Die Modellliste konnte nicht geladen werden: " + m_errorMessage );
      return m_control;
    }

    final Label mainImageLabel = new Label( m_control, SWT.NONE );
    final GridData mainLabelgridData = new GridData();
    mainLabelgridData.horizontalSpan = 2;
    mainLabelgridData.horizontalAlignment = GridData.CENTER;
    mainLabelgridData.grabExcessHorizontalSpace = true;
    mainLabelgridData.verticalSpan = 1;
    mainLabelgridData.verticalAlignment = GridData.FILL;
    mainImageLabel.setLayoutData( mainLabelgridData );

    try
    {
      final String mainImageName = m_modellist.getMainImage();
      final URL mainImageURL = new URL( m_location, mainImageName );
      final Image mainImage = new Image( display, mainImageURL.openStream() );
      mainImageLabel.setImage( mainImage );
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }

    final Label headingLabel = new Label( m_control, SWT.SINGLE );
    final GridData headingGridData = new GridData( GridData.BEGINNING, GridData.BEGINNING, false,
        false, 2, 2 );
    headingLabel.setLayoutData( headingGridData );
    final Font headingFont = FontUtilities.createChangedFontData( headingLabel.getFont()
        .getFontData(), 10, SWT.BOLD, headingLabel.getDisplay() );
    headingLabel.setFont( headingFont );
    headingLabel.setBackground( display.getSystemColor( SWT.COLOR_WHITE ) );
    m_disposeFonts.add( headingFont );
    headingLabel.setText( "Bitte wählen Sie das Einzugsgebiet" );

    final List list = new List( m_control, SWT.SINGLE );
    final GridData listGridData = new GridData( GridData.BEGINNING, GridData.BEGINNING, false,
        false );
    list.setLayoutData( listGridData );
    final Font listfont = FontUtilities.createChangedFontData( list.getFont().getFontData(), 10,
        SWT.NONE, list.getDisplay() );
    list.setFont( listfont );
    m_disposeFonts.add( listfont );

    final ListViewer viewer = new ListViewer( list );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( m_labelProvider );

    m_imageLabel = new Label( m_control, SWT.NONE );
    m_imageLabel.setLayoutData( new GridData() );

    viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final ModellistType.ModelType model = (ModelType)( (IStructuredSelection)event
            .getSelection() ).getFirstElement();
        
        setModel( model );
      }
    } );

    // create content
    viewer.setInput( m_modellist.getModel() );
    viewer.setSelection( new StructuredSelection( m_modellist.getModel().get( 0 ) ) );
    display.asyncExec( new Runnable()
    {
      public void run()
      {
        viewer.refresh();
      }
    } );

    return m_control;
  }
  
  public void setModel( final ModellistType.ModelType model )
  {
    m_model = model;
    
    final Image oldImage = m_imageLabel.getImage();

    ImageData imageData = (ImageData)m_imageHash.get( model );
    if( imageData == null )
    {
      try
      {
        final URL imageURL = new URL( m_location, model.getImage() );
        imageData = new ImageData( imageURL.openStream() );
        m_imageHash.put( model, imageData );
      }
      catch( MalformedURLException e )
      {
        e.printStackTrace();
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
    }

    final Image newImage = imageData == null ? null : new Image( m_control.getDisplay(), imageData ); 
    m_imageLabel.setImage( newImage );

    if( oldImage != null )
      oldImage.dispose();

    m_control.layout();
    m_control.redraw();
  }

  public String getModel()
  {
    return m_model.getName();
  }

}