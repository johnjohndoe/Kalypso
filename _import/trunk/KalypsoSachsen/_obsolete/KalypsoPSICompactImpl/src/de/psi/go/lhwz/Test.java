/*
 * Created on 19.05.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package de.psi.go.lhwz;

import java.io.File;
import java.util.Date;

/**
 * @author BKarpa
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class Test implements PSICompact{
    
        /* (non-Javadoc)
         * @see de.psi.go.lhwz.PSICompact#init()
         */
    public void init()
    {
        // TODO Auto-generated method stub
    }
    
        /* (non-Javadoc)
         * @see de.psi.go.lhwz.PSICompact#distributeFile(java.io.File)
         */
    public boolean distributeFile(File file) throws ECommException {
        // TODO Auto-generated method stub
        return false;
    }
    
        /* (non-Javadoc)
         * @see de.psi.go.lhwz.PSICompact#removeFile(java.io.File)
         */
    public boolean removeFile(File file) throws ECommException {
        // TODO Auto-generated method stub
        return false;
    }
    
        /* (non-Javadoc)
         * @see de.psi.go.lhwz.PSICompact#writeProtocol(java.lang.String)
         */
    public boolean writeProtocol(String message) throws ECommException {
        // TODO Auto-generated method stub
        return false;
    }
    
        /* (non-Javadoc)
         * @see de.psi.go.lhwz.PSICompact#getInfo(int)
         */
    public ObjectInfo[] getInfo(int typespec) throws ECommException {
        ObjectInfo o1 = new ObjectInfo("HN.501ES.02PG...550190", "Pegel 550190");
        ObjectInfo o2 = new ObjectInfo("HN.501ES.02PG...550192", "Pegel 550192");
        ObjectInfo o3 = new ObjectInfo("HN.501ES.01OM...O_112", "Ombrometer 112");
        ObjectInfo[] v1 = {o1,o2,o3};
        return v1;
    }
    
        /* (non-Javadoc)
         * @see de.psi.go.lhwz.PSICompact#getArchiveData(java.lang.String, java.util.Date, java.util.Date)
         */
    public ArchiveData[] getArchiveData(String id, int arcType, Date from, Date to) throws ECommException {
        // TODO Auto-generated method stub
        return null;
    }
    
        /* (non-Javadoc)
         * @see de.psi.go.lhwz.PSICompact#setArchiveData(java.lang.String, java.util.Date, java.util.Date, de.psi.go.lhwz.PSICompact.ArchiveData)
         */
    public boolean setArchiveData(String id, int arcType, Date from, ArchiveData[] data) throws ECommException {
        // TODO Auto-generated method stub
        return false;
    }
    
        /* (non-Javadoc)
         * @see de.psi.go.lhwz.PSICompact#getWQParams(java.lang.String)
         */
    public WQParamSet[] getWQParams(String id) throws ECommException {
        // TODO Auto-generated method stub
        return null;
    }
    
    /**
     * Lesen Meta Daten für ein Objekt
     *
     * @param id Kennzeichen des Objekts für das Meta Daten geholt werden sollen.
     * @return die Meta Daten dieses Objekt, oder null, wenn ein Fehler aufgetreten ist.
     */
    public ObjectMetaData getObjectMetaData(String id) throws ECommException {
        // TODO Auto-generated method stub
        return null;
    }
    
    /**
     * Liefert alle Benutzerrechte für einen gegebenen Benutzer und eine Benutzerklasse
     *
     * @param userId Id des eingeloggten Benutzers
     * @param userClass Benutzerklasse des eingeloggten Benutzers
     * @return Liste der Benutzerklassen, die für diesen Benutzer definiert wurden,
     * oder null wenn Benutzer nicht definiert, bzw. leeren Array, wenn keine
     * Benutzerklassen für diesen Benutzer parametriert sind.
     * Text
     */
    public String[] getUserRights(String userId, String userClass) throws ECommException {
        return null;
    }
    
    /**
     * Liefert alle Benutzerrechte für einen gegebenen Benutzer und eine Benutzerklasse
     *
     * @param userId Id des eingeloggten Benutzers
     * @param userClass Benutzerklasse des eingeloggten Benutzers
     * @return Liste der Benutzerrechte, die für diesen Benutzer definiert wurden,
     * oder null wenn Benutzer nicht definiert, bzw. leeren Array, wenn keine
     * Benutzerrechte für diesen Benutzer parametriert sind.
     * Text
     */
    public String[] getUserClasses(String userId) throws ECommException {
        return null;
    }
    
    /**
     * Liefert den Typ einer Messung
     *
     * @param id Kennzeichen des Objekts für das der Typ geholt werden sollen.
     * @return Messungstyp, siehe auch MEAS_* Konstanten, MEAS_UNDEF wird geliefert wenn das Objekt
     *      nicht existiert oder ein anderer Fehler aufgetreten ist
     */
    public int getMeasureType(java.lang.String id) throws ECommException {

        if (id.equals("HN.501ES.02PG...550190") || id.equals("HN.501ES.02PG...550192")) {
            return MEAS_LEVEL;
        } else if (id.equals("HN.501ES.01OM...O_112")) {
            return MEAS_RAINFALL;
        }
        return MEAS_UNDEF;
    }

    /**
     * @see de.psi.go.lhwz.PSICompact#distributeFile(java.lang.String)
     */
    public boolean distributeFile( String filename ) throws ECommException
    {
      return false;
    }

    /**
     * @see de.psi.go.lhwz.PSICompact#removeFile(java.lang.String)
     */
    public boolean removeFile( String filename ) throws ECommException
    {
      return false;
    }

    /**
     * @see de.psi.go.lhwz.PSICompact#getDataModelVersion()
     */
    public int getDataModelVersion() throws ECommException
    {
      return 0;
    }
    
}
