package org.kalypso.ui.view.prognose;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
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
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
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
  // TODO

  // fonts

  // images

  // control
  }

  public Composite createControl( final Composite parent )
  {
    final Display display = parent.getDisplay();
    final FormToolkit toolkit = new FormToolkit( display );
    final Form form = toolkit.createForm( parent );

    final GridLayout gridLayout = new GridLayout( 2, false );
    form.getBody().setLayout( gridLayout );
    final GridData formGridData = new GridData( GridData.FILL_BOTH );
    formGridData.horizontalAlignment = GridData.CENTER;
    form.setLayoutData( formGridData );

    if( m_modellist == null )
    {
      final Label label = new Label( parent, SWT.CENTER );
      label.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      label.setText( "Die Modellliste konnte nicht geladen werden: " + m_errorMessage );
      return form;
    }

    final Label mainImageLabel = toolkit.createLabel( form.getBody(), null, SWT.NONE );
    final GridData mainLabelgridData = new GridData();
    mainLabelgridData.horizontalSpan = 2;
    mainLabelgridData.horizontalAlignment = GridData.CENTER;
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

    final Label headingLabel = new Label( form.getBody(), SWT.SINGLE );
    final GridData headingGridData = new GridData( GridData.BEGINNING, GridData.BEGINNING, false,
        false, 2, 2 );
    headingLabel.setLayoutData( headingGridData );
    final Font headingFont = FontUtilities.createChangedFontData( headingLabel.getFont()
        .getFontData(), 10, SWT.BOLD, headingLabel.getDisplay() );
    headingLabel.setFont( headingFont );
    m_disposeFonts.add( headingFont );
    headingLabel.setText( "Bitte wählen Sie ein Vorhersagegebiet" );

    // TODO
    final List list = new List( form.getBody(), SWT.SINGLE );
    final GridData listGridData = new GridData( GridData.BEGINNING, GridData.BEGINNING, false,
        false );
    list.setLayoutData( listGridData );
    list.setToolTipText( "Doppelklick startet die Prognoserechnung" );
    final Font listfont = FontUtilities.createChangedFontData( list.getFont().getFontData(), 10,
        SWT.NONE, list.getDisplay() );
    list.setFont( listfont );
    m_disposeFonts.add( listfont );

    final ListViewer viewer = new ListViewer( list );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( m_labelProvider );

    final Label imageLabel = toolkit.createLabel( form.getBody(), null, SWT.NONE );
    imageLabel.setLayoutData( new GridData() );

    //    m_button = toolkit.createButton( form.getBody(), "Prognoserechnung
    // starten", SWT.PUSH );
    //    m_button.setEnabled( false );

    //    // event handling
    //    viewer.addDoubleClickListener( new IDoubleClickListener()
    //    {
    //      public void doubleClick( DoubleClickEvent event )
    //      {
    //        startModel( event.getSelection() );
    //      }
    //    } );

    //    final Control button = m_button;
    viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final ModellistType.ModelType model = (ModelType)( (IStructuredSelection)event
            .getSelection() ).getFirstElement();

        //        button.setEnabled( model != null );

        final Image oldImage = imageLabel.getImage();

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

        final Image newImage = imageData == null ? null : new Image( display, imageData ); 
        imageLabel.setImage( newImage );

        if( oldImage != null )
          oldImage.dispose();

        form.getBody().layout();
        form.getBody().redraw();
      }
    } );

    //    m_button.addSelectionListener( new SelectionAdapter()
    //    {
    //      public void widgetSelected( final SelectionEvent e )
    //      {
    //        startModel( viewer.getSelection() );
    //      }
    //    } );

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

    return form;
  }

}