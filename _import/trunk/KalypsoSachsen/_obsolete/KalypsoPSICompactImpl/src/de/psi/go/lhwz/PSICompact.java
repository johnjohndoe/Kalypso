/*
 * 
 * To change the template for this generated file go to Window - Preferences -
 * Java - Code Generation - Code and Comments
 */
package de.psi.go.lhwz;

import java.io.File;
import java.io.Serializable;
import java.util.Date;

/**
 * Interface für Zugriffe auf PSICompact
 * 
 * @author BKarpa To change the template for this generated type comment go to
 *         Window - Preferences - Java - Code Generation - Code and Comments
 */
public interface PSICompact
{
  // Definition der unterschiedlichen Status Konstanten des Archivs
  /** Archiv Status Undefiniert */
  public static int STATUS_UNDEF = -1;

  /** Archiv Status OK */
  public static int STATUS_OK = 0;

  /**
   * Archiv Status Der Archivwert ist manuell korrigiert worden, d.h. das
   * Ergebnis der automatischen Verdichtung ist überschrieben worden. Der Wert
   * kann für die Prognose verwendet werden.
   */
  public static int STATUS_MANKOR = 1;

  /**
   * Archiv Status Mindestens ein Ersatzwert ist in die automatische Wertbildung
   * eingeflossen, d.h. der Wert ist für die Prognose u.U. nicht verwendbar.
   */
  public static int STATUS_ERSALLG = 2;

  /**
   * Archiv Status Der Archivwert wurde durch die Simulation, bzw. Prognose
   * gebildet. Dieser Status wird automatisch gesetzt, wenn Archivwerte durch
   * die Prognose geschrieben wurden.
   */
  public static int STATUS_REKO = 3;

  /**
   * Archiv Status Der Archivwert wurde durch eine automatische Nachberechnung
   * gebildet. Der Wert kann für die Prognose verwendet werden.
   */
  public static int STATUS_AUTO = 4;

  /**
   * Archiv Status Der Archivwert wurde automatisch nachgeführt. Die
   * automatische Nachführung sorgt beim Start des Archivdienstes automatisch
   * dafür, dass nicht vorhandene Archivwerte erzeugt werden. Wie die
   * Archivwerte erzeugt werden, richtet sich nach der parametrierten
   * Nachführstrategie dieses Objektes. Der Wert kann für die Prognose u.U.
   * nicht verwendet werden.
   */
  public static int STATUS_NACH = 5;

  /**
   * Archiv Status Ein Archivwert wird als normiert bezeichnet, wenn er nicht
   * existiert, d.h. nicht oder noch nicht geschrieben wurde. Der Wert kann für
   * die Prognose nicht verwendet werden.
   */
  public static int STATUS_NORM = 6;

  /**
   * Archiv Status Mindestens ein normierter Archivwert ist in die Wertbildung
   * während der Verdichtung eingeflossen. Der Wert kann für die Prognose u.U.
   * nicht verwendet werden.
   */
  public static int STATUS_NORMALLG = 7;

  // Definition der unterschiedlichen Archivtypen
  /** Archiv Undefiniert */
  public static int ARC_UNDEF = -1;

  /** Archiv 5 Minuten */
  public static int ARC_MIN5 = 0;

  /** Archiv 15 Minuten */
  public static int ARC_MIN15 = 1;

  /** Archiv Stunde */
  public static int ARC_HOUR = 2;

  /** Archiv Tag */
  public static int ARC_DAY = 3;

  /** Archiv Monat */
  public static int ARC_MONTH = 4;

  // Definition der unterschiedlichen Objekt Typen
  // TYPE_MEASUREMENT bezeichnet eine Messung und somit die Eingangsdaten für
  // die Prognose
  // TYPE_VALUE bezeichnet einen Messwert und somit die Ausgangsdaten der
  // Prognose

  /** Typ Undefiniert */
  public static int TYPE_UNDEF = -1;

  /** Typ gemessener Wert */
  public static int TYPE_MEASUREMENT = 0;

  /** Typ Vorhergesagter Wert */
  public static int TYPE_VALUE = 1;

  // Definition der unterschiedlichen SI Einheiten
  // SI_NO_UNIT bezeichnet Werte ohne Einheit
  // Weitere Einheiten können bei Bedarf ergänzt werden

  /** SI Einheit: Undefiniert */
  public static int SI_UNDEF = -1;

  /** SI Einheit: Keine Einheit */
  public static int SI_NO_UNIT = 0;

  /** SI Einheit: Meter */
  public static int SI_METER = 1;

  /** SI Einheit: Kubikmeter */
  public static int SI_QUBIC_METER = 2;

  /** SI Einheit: Kubikmeter pro Sekunde */
  public static int SI_CUBIC_METER_PER_SECOND = 3;

  /** SI Einheit: Kelvin */
  public static int SI_KELVIN = 4;

  // Definition der unterschiedlichen Messungstypen
  /** Messungstyp: Undefiniert */
  public static int MEAS_UNDEF = -1;

  /** Messungstyp: Niederschlag */
  public static int MEAS_RAINFALL = 0;

  /** Messungstyp: Temperatur */
  public static int MEAS_TEMPERATUR = 1;

  /** Messungstyp: Wasserstand */
  public static int MEAS_LEVEL = 2;

  /** Messungstyp: Durchfluss */
  public static int MEAS_FLOW = 3;

  /** öffentliche Klasse für die Klartext Informationen */
  public class ObjectInfo implements Serializable
  {

    /** Dass Kennzeichen des Objekts */
    private String id = null;

    /** Die Bezeichnung/Beschreibung des Objekts */
    private String description = null;

    // constructors

    /** empty constructor */
    public ObjectInfo()
    {
      id = "";
      description = "";
    }

    /**
     * complete constructor
     * 
     * @param idParam
     *          the id to set
     * @param descriptionParam
     *          the description to set
     */
    public ObjectInfo( String idParam, String descriptionParam )
    {
      id = idParam;
      description = descriptionParam;
    }

    // getter & setter

    /**
     * generated get method
     * 
     * @return Returns the id.
     */
    public final String getId()
    {
      return id;
    }

    /**
     * generated set method
     * 
     * @param id
     *          The id to set.
     */
    public final void setId( String id )
    {
      this.id = id;
    }

    /**
     * generated get method
     * 
     * @return Returns the description.
     */
    public final String getDescription()
    {
      return description;
    }

    /**
     * generated set method
     * 
     * @param description
     *          The description to set.
     */
    public final void setDescription( String description )
    {
      this.description = description;
    }
  }

  /**
   * öffentliche Klasse für die Objekt Meta Informationen
   */
  public class ObjectMetaData implements Serializable
  {

    /** Dass Kennzeichen des Objekts */
    private String id = null;

    /** Die SI Einheit des Objekts */
    private int unit = SI_UNDEF;

    /** Die Alarmstufe 1 des Objekts in SI Einheit */
    private double alarm1 = 0.0;

    /** Die Alarmstufe 2 des Objekts in SI Einheit */
    private double alarm2 = 0.0;

    /** Die Alarmstufe 3 des Objekts in SI Einheit */
    private double alarm3 = 0.0;

    /** Die Alarmstufe 4 des Objekts in SI Einheit */
    private double alarm4 = 0.0;

    /** Messtischblattnummer */
    private int mapNo = 0;

    /** Rechtswert Geo Koordinate */
    private int right = 0;

    /** Hochwert Geo Koordinate */
    private int height = 0;

    /** Flussgebiet */
    private String system = null;

    /** Fluss/Gewaesser */
    private String river = null;

    /**
     * die Liste von unterstützten Archiven für dieses Objekt, siehe ARC_*
     * Konstanten
     */
    private int[] archiveData = null;

    /** Pegelnullpunkt */
    private double level = 0.0;

    /** Höhenangabe des Pegelnullpunktes */
    private String levelUnit = null;

    // constructors

    /** empty constructor */
    public ObjectMetaData()
    {
      id = null;
      unit = SI_UNDEF;
      alarm1 = 0.0;
      alarm2 = 0.0;
      alarm3 = 0.0;
      alarm4 = 0.0;
      mapNo = 0;
      right = 0;
      height = 0;
      archiveData = null;
      level = 0.0;
      levelUnit = null;
      system = null;
      river = null;
    }

    /**
     * @param unitParam
     * @param alarm1Param
     * @param alarm2Param
     * @param alarm3Param
     * @param alarm4Param
     * @param mapNoParam
     * @param rightParam
     * @param heightParam
     * @param archiveDataParam
     * @param idParam
     */
    public ObjectMetaData( String idParam, int unitParam, double alarm1Param, double alarm2Param,
        double alarm3Param, double alarm4Param, int mapNoParam, int rightParam, int heightParam,
        int[] archiveDataParam, double levelParam, String levelUnitParam )
    {
      id = idParam;
      unit = unitParam;
      alarm1 = alarm1Param;
      alarm2 = alarm2Param;
      alarm3 = alarm3Param;
      alarm4 = alarm4Param;
      mapNo = mapNoParam;
      right = rightParam;
      height = heightParam;
      archiveData = archiveDataParam;
      level = levelParam;
      levelUnit = levelUnitParam;
    }

    // getter & setter

    /** @return Returns the id. */
    public final String getId()
    {
      return id;
    }

    /**
     * @param id
     *          The id to set.
     */
    public final void setId( String id )
    {
      this.id = id;
    }

    /** @return Returns the unit. */
    public final int getUnit()
    {
      return unit;
    }

    /**
     * @param unit
     *          The unit to set.
     */
    public final void setUnit( int unit )
    {
      this.unit = unit;
    }

    /** @return Returns the alarm1. */
    public final double getAlarm1()
    {
      return alarm1;
    }

    /**
     * @param alarm1
     *          The alarm1 to set.
     */
    public final void setAlarm1( double alarm1 )
    {
      this.alarm1 = alarm1;
    }

    /** @return Returns the alarm2. */
    public final double getAlarm2()
    {
      return alarm2;
    }

    /**
     * @param alarm2
     *          The alarm2 to set.
     */
    public final void setAlarm2( double alarm2 )
    {
      this.alarm2 = alarm2;
    }

    /** @return Returns the alarm3. */
    public final double getAlarm3()
    {
      return alarm3;
    }

    /**
     * @param alarm3
     *          The alarm3 to set.
     */
    public final void setAlarm3( double alarm3 )
    {
      this.alarm3 = alarm3;
    }

    /** @return Returns the alarm4. */
    public final double getAlarm4()
    {
      return alarm4;
    }

    /**
     * @param alarm4
     *          The alarm4 to set.
     */
    public final void setAlarm4( double alarm4 )
    {
      this.alarm4 = alarm4;
    }

    /**
     * Getter for property mapNo.
     * 
     * @return Value of property mapNo.
     */
    public int getMapNo()
    {
      return mapNo;
    }

    /**
     * Setter for property mapNo.
     * 
     * @param mapNo
     *          New value of property mapNo.
     */
    public void setMapNo( int mapNo )
    {
      this.mapNo = mapNo;
    }

    /**
     * Getter for property right.
     * 
     * @return Value of property right.
     */
    public int getRight()
    {
      return right;
    }

    /**
     * Setter for property right.
     * 
     * @param right
     *          New value of property right.
     */
    public void setRight( int right )
    {
      this.right = right;
    }

    /**
     * Getter for property height.
     * 
     * @return Value of property height.
     */
    public int getHeight()
    {
      return height;
    }

    /**
     * Setter for property height.
     * 
     * @param height
     *          New value of property height.
     */
    public void setHeight( int height )
    {
      this.height = height;
    }

    /**
     * Getter for property archiveData.
     * 
     * @return Value of property archiveData.
     */
    public int[] getArchiveData()
    {
      return this.archiveData;
    }

    /**
     * Setter for property archiveData.
     * 
     * @param archiveData
     *          New value of property archiveData.
     */
    public void setArchiveData( int[] archiveData )
    {
      this.archiveData = archiveData;
    }

    /**
     * Getter for property level.
     * 
     * @return Value of property level.
     */
    public double getLevel()
    {
      return level;
    }

    /**
     * Setter for property level.
     * 
     * @param level
     *          New value of property level.
     */
    public void setLevel( double level )
    {
      this.level = level;
    }

    /**
     * Getter for property levelUnit.
     * 
     * @return Value of property levelUnit.
     */
    public java.lang.String getLevelUnit()
    {
      return levelUnit;
    }

    /**
     * Setter for property levelUnit.
     * 
     * @param levelUnit
     *          New value of property levelUnit.
     */
    public void setLevelUnit( java.lang.String levelUnit )
    {
      this.levelUnit = levelUnit;
    }

    /**
     * Setter for property system.
     * 
     * @param system
     *          New value of property system ( Flussgebiet).
     */
    public void setRiversystem( java.lang.String riversystem )
    {
      this.system = riversystem;
    }

    /**
     * Getter for property system.
     * 
     * @return Value of property system ( Flussgebiet).
     */
    public java.lang.String getRiversystem()
    {
      return system;
    }

    /**
     * Setter for property river.
     * 
     * @param system
     *          New value of property river ( Fluss / Gewaesser).
     */
    public void setRiver( java.lang.String river )
    {
      this.river = river;
    }

    /**
     * Getter for property river.
     * 
     * @return Value of property river ( Fluss / Gewaesser).
     */
    public java.lang.String getRiver()
    {
      return river;
    }
  }

  /** öffentliche Klasse für die Archivwerte */
  public class ArchiveData implements Serializable
  {
    /** Zeitstempel des Archivwertes */
    private Date Timestamp;

    /** Status des Archivwertes, siehe Konstanten ARC_* */
    private int status;

    /** Der Archivwert in SI Einheit */
    private double value;

    // constructors
    /** empty constructor */
    public ArchiveData()
    {
      // Timestamp = ;
      status = STATUS_UNDEF;
      value = 0.0;
    }

    /**
     * @param tsParam
     * @param statusParam
     * @param valueParam
     */
    public ArchiveData( Date tsParam, int statusParam, double valueParam )
    {
      Timestamp = tsParam;
      status = statusParam;
      value = valueParam;
    }

    // getter & setter

    /** @return Returns the status. */
    public final int getStatus()
    {
      return status;
    }

    /**
     * @param status
     *          The status to set.
     */
    public final void setStatus( int status )
    {
      this.status = status;
    }

    /** @return Returns the timestamp. */
    public final Date getTimestamp()
    {
      return Timestamp;
    }

    /**
     * @param timestamp
     *          The timestamp to set.
     */
    public final void setTimestamp( Date timestamp )
    {
      Timestamp = timestamp;
    }

    /** @return Returns the value. */
    public final double getValue()
    {
      return value;
    }

    /**
     * @param value
     *          The value to set.
     */
    public final void setValue( double value )
    {
      this.value = value;
    }
  }

  /** öffentliche Klasse für die W-Q Parameter */
  public class WQData implements Serializable
  {
    /** obere Wasserstandsgrenze in cm */
    private double WGR;

    /** Konstante W1 */
    private double W1;

    /** Konstante LNK1 */
    private double LNK1;

    /** Konstante K2 */
    private double K2;

    // constructors

    /** empty constructor */
    public WQData()
    {
      WGR = 0.0;
      W1 = 0.0;
      LNK1 = 0.0;
      K2 = 0.0;
    }

    /**
     * @param WGRParam
     * @param W1Param
     * @param LNK1Param
     * @param K2Param
     */
    public WQData( double WGRParam, double W1Param, double LNK1Param, double K2Param )
    {
      WGR = WGRParam;
      W1 = W1Param;
      LNK1 = LNK1Param;
      K2 = K2Param;
    }

    // getter & setter

    /** @return Returns the k2. */
    public final double getK2()
    {
      return K2;
    }

    /**
     * @param k2
     *          The k2 to set.
     */
    public final void setK2( double k2 )
    {
      K2 = k2;
    }

    /** @return Returns the lNK1. */
    public final double getLNK1()
    {
      return LNK1;
    }

    /**
     * @param lnk1
     *          The lNK1 to set.
     */
    public final void setLNK1( double lnk1 )
    {
      LNK1 = lnk1;
    }

    /** @return Returns the w1. */
    public final double getW1()
    {
      return W1;
    }

    /**
     * @param w1
     *          The w1 to set.
     */
    public final void setW1( double w1 )
    {
      W1 = w1;
    }

    /** @return Returns the wGR. */
    public final double getWGR()
    {
      return WGR;
    }

    /**
     * @param wgr
     *          The wGR to set.
     */
    public final void setWGR( double wgr )
    {
      WGR = wgr;
    }
  }

  /** öffenliche Klasse für einen W-Q Parametersatz */
  public class WQParamSet implements Serializable
  {
    /** Gültigkeitszeiraum ab */
    private Date validFrom;

    /** die Parameter */
    private WQData[] wqData;

    // constructors
    /**
     * @param validFrom
     * @param wqData
     */
    public WQParamSet( Date validFrom, WQData[] wqData )
    {
      super();
      this.validFrom = validFrom;
      this.wqData = wqData;
    }

    // getter & setter

    /** @return Returns the validFrom. */
    public final Date getValidFrom()
    {
      return validFrom;
    }

    /**
     * @param validFrom
     *          The validFrom to set.
     */
    public final void setValidFrom( Date validFrom )
    {
      this.validFrom = validFrom;
    }

    /** return Returns the wqData. */
    public final WQData[] getWqData()
    {
      return wqData;
    }

    /**
     * @param wqData
     *          The wqData to set.
     */
    public final void setWqData( WQData[] wqData )
    {
      this.wqData = wqData;
    }
  }

  /** Initialisieren der Schnittstelle zu PSICompact */
  public void init() throws ECommException;

  /**
   * Verteilen einer Datei
   * 
   * @param file
   *          relativer Pfad und Dateiname der Datei die verteilt werden soll.
   * @return True falls alles OK, False wenn Fehler aufgetreten ist
   */
  public boolean distributeFile( String filename ) throws ECommException;

  /**
   * Löschen einer Datei
   * 
   * @param filename
   *          relativer Pfad und Dateiname der Datei die gelöscht werden soll.
   * @return True falls alles OK, False wenn Fehler aufgetreten ist
   */
  public boolean removeFile( String filename ) throws ECommException;

  /**
   * Protokoll Eintrag schreiben
   * 
   * @param message
   *          Nachricht die protokolliert werden soll.
   * @return True falls alles OK, False wenn Fehler aufgetreten ist
   */
  public boolean writeProtocol( String message ) throws ECommException;

  /**
   * Liest die aktuelle Datenbestandsversionsnummer
   * 
   * @return Datenbestandsversionsnummer
   * @throws ECommException
   */
  public int getDataModelVersion() throws ECommException;

  /**
   * Klartext Informationen zu einem Objekttyp lesen
   * 
   * @param typespec
   *          Objekttyp zu dem die Daten geholt werden sollen siehe Konstanten
   *          TYPE_*.
   * @return die Liste von Objektbeschreibungen für diesen Objekttyp, oder null,
   *         wenn ein Fehler aufgetreten ist.
   */
  public ObjectInfo[] getInfo( int typespec ) throws ECommException;

  /**
   * Lesen von Archivwerten für ein Objekt
   * 
   * @param id
   *          Kennzeichen des Objekts für das Archivadaten abgefragt werden
   *          sollen.
   * @param arcType
   *          Archivtyp aus dem die Archivwerte geholt werden sollen, siehe
   *          Konstanten ARC_*
   * @param from
   *          erster Archiveintrag der geholt werden soll
   * @param to
   *          letzter Archiveintrag der geholt werden soll
   * @return die Liste von Archivdaten für dieses Objekt, oder null, wenn ein
   *         Fehler aufgetreten ist.
   */
  public ArchiveData[] getArchiveData( String id, int arcType, Date from, Date to )
      throws ECommException;

  /**
   * Schreiben von Archivwerten für ein Objekt
   * 
   * @param id
   *          Kennzeichen des Objekts für das Archivadaten geschrieben werden
   *          sollen.
   * @param arcType
   *          Archivtyp aus dem die Archivwerte geholt werden sollen, siehe
   *          Konstanten ARC_*
   * @param from
   *          erster Archiveintrag der geschrieben werden soll.
   * @param data
   *          die zu schreibenden Archivwerte
   * @return True falls alles OK, False wenn Fehler aufgetreten ist.
   */
  public boolean setArchiveData( String id, int arcType, Date from, ArchiveData[] data )
      throws ECommException;

  /**
   * Lesen von W-Q Parametern für ein Objekt
   * 
   * @param id
   *          Kennzeichen des Objekts für das W-Q Parameter geholt werden
   * @return die Liste von W-Q Parametern für dieses Objekt, oder null, wenn ein
   *         Fehler aufgetreten ist.
   */
  public WQParamSet[] getWQParams( String id ) throws ECommException;

  /**
   * Lesen der Meta Daten für ein Objekt
   * 
   * @param id
   *          Kennzeichen des Objekts für das Meta Daten geholt werden sollen.
   * @return die Meta Daten dieses Objekt, oder null, wenn ein Fehler
   *         aufgetreten ist.
   */
  public ObjectMetaData getObjectMetaData( String id ) throws ECommException;

  /**
   * Liefert alle Benutzerklassen für einen gegebenen Benutzer
   * 
   * @param userId
   *          Id des eingeloggten Benutzers
   * @return Liste der Benutzerklassen, die für diesen Benutzer definiert
   *         wurden, oder null wenn Benutzer nicht definiert, bzw. leeren Array,
   *         wenn keine Benutzerklassen für diesen Benutzer parametriert sind.
   *         Text
   */
  public String[] getUserClasses( String userId ) throws ECommException;

  /**
   * Liefert alle Benutzerrechte für einen gegebenen Benutzer und eine
   * Benutzerklasse
   * 
   * @param userId
   *          Id des eingeloggten Benutzers
   * @param userClass
   *          Benutzerklasse des eingeloggten Benutzers
   * @return Liste der Benutzerrechte, die für diesen Benutzer definiert wurden,
   *         oder null wenn Benutzer nicht definiert, bzw. leeren Array, wenn
   *         keine Benutzerrechte für diesen Benutzer parametriert sind. Text
   */
  public String[] getUserRights( String userId, String userClass ) throws ECommException;

  /**
   * Liefert den Typ einer Messung
   * 
   * @param id
   *          Kennzeichen des Objekts für das der Typ geholt werden sollen.
   * @return Messungstyp, siehe auch MEAS_* Konstanten, MEAS_UNDEF wird
   *         geliefert wenn das Objekt nicht existiert oder ein anderer Fehler
   *         aufgetreten ist
   */
  public int getMeasureType( java.lang.String id ) throws ECommException;

  /**
   * Schnittstelle zum DMS
   */

  /**
   * Liefert alle Benutzer für eine gegebene Benutzerklasse
   * 
   * @param userClass
   *          Benutzerklasse
   * @return Liste der Benutzer, die für diese Benutzerklasse definiert wurden,
   *         oder null wenn Benutzerklasse nicht definiert, bzw. leeres Array,
   *         wenn keine Benutzer für diese Benutzerklasse vorhanden sind.
   */
  public String[] getUser( String userClass ) throws ECommException;

  /**
   * Liefert die Rechnernummer (1 oder 2) auf dem die DB Dienstgruppe aktiv ist
   * 
   * @return 0, wenn keine Dienstgruppe aktiv
   */
  public int getActivDBGroup() throws ECommException;

  /**
   * Einstellen einer Datei in die Fileverteilung
   * 
   * @param lokaler
   *          Pfad und Dateiname der Datei die verteilt werden soll.
   * @param relativer
   *          Pfad und Dateiname der Datei in der Fileverteilung.
   * @return True falls alles OK, False wenn Fehler aufgetreten ist
   */
  public boolean copyanddistributeFile( File source, String destination ) throws ECommException;

  /**
   * TODO: Schnittstelle zum Informationsverteiler Realisierungspriorität von
   * oben nach unten :-)
   */

  /**
   * Objektinfo für einen Pegel
   * 
   * @param Pegelkennziffer
   *          (letzten n Stellen).
   * @return Bemerkung für diesen Pegel, die dem Pegelmelder vorgelesen werden
   *         soll
   */
  public String getComment( String Pegelkennziffer ) throws ECommException;

  /**
   * Gewaesser für einen Pegel
   * 
   * @param Pegelkennziffer
   *          (letzten n Stellen).
   * @return Gewaesser für diesen Pegel
   */
  public String getGewaesser( String Pegelkennziffer ) throws ECommException;

  /**
   * Flussgebiet für einen Pegel
   * 
   * @param Pegelkennziffer
   *          (letzten n Stellen).
   * @return Flussgebiet für diesen Pegel
   */
  public String getFlussgebiet( String Pegelkennziffer ) throws ECommException;
}