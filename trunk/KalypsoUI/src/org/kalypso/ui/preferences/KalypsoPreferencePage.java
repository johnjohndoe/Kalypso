package org.kalypso.ui.preferences;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * This class represents a preference page that is contributed to the
 * Preferences dialog. By subclassing <samp>FieldEditorPreferencePage </samp>,
 * we can use the field support built into JFace that allows us to create a page
 * that is small and knows how to save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They are stored in the
 * preference store that belongs to the main plug-in class. That way,
 * preferences can be accessed directly via the preference store.
 */

public class KalypsoPreferencePage extends FieldEditorPreferencePage implements
    IWorkbenchPreferencePage
{
  public KalypsoPreferencePage()
  {
    super( GRID );
    setPreferenceStore( KalypsoGisPlugin.getDefault().getPreferenceStore() );
    setDescription( "Hier k�nnen Sie die Kalypso-Einstellungen �ndern" );
  }

  /**
   * Creates the field editors. Field editors are abstractions of the common GUI
   * blocks needed to manipulate various types of preferences. Each field editor
   * knows how to save and restore itself.
   */
  public void createFieldEditors()
  {
    addField( new StringFieldEditor( IKalypsoPreferences.CLIENT_CONF_URLS, "Verf�gbare &Server (Komma-getrennte Liste):", getFieldEditorParent() ) );

    addField( new StringFieldEditor( IKalypsoPreferences.NUMBER_OF_DAYS, "&Tagesanzahl (Zeitreihenbrowser):", getFieldEditorParent() ) );

    addField( new BooleanFieldEditor( IKalypsoPreferences.HTTP_PROXY_USE, "Http-&Proxy benutzen", getFieldEditorParent() ) );
    addField( new StringFieldEditor( IKalypsoPreferences.HTTP_PROXY_HOST, "Http-Proxy &Hostname:", getFieldEditorParent() ) );
    addField( new StringFieldEditor( IKalypsoPreferences.HTTP_PROXY_PORT, "Http-Proxy Port&nummer:", getFieldEditorParent() ) );
    addField( new StringFieldEditor( IKalypsoPreferences.HTTP_PROXY_USER, "Http-Proxy &Benutzername:", getFieldEditorParent() ) );
    addField( new StringFieldEditor( IKalypsoPreferences.HTTP_PROXY_PASS, "Http-Proxy Pass&wort:", getFieldEditorParent() ) );
    
//
//    addField( new RadioGroupFieldEditor( P_CHOICE, "An example of a multiple-choice preference", 1,
//        new String[][]
//        {
//        { "&Choice 1", "choice1" },
//        { "C&hoice 2", "choice2" } }, getFieldEditorParent() ) );
  }

  public void init( IWorkbench workbench )
  {
    // empty
  }
}