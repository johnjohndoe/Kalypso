/**
 * I_Filter.java
 *
 * @author Christoph Küpferle
 */

package de.tuhh.kalypso.data;

import java.io.File;
import java.util.HashSet;
import java.util.Hashtable;

public interface I_Filter
{
    public void importASCIIFilesToDb( File  asciiSource[] , String xmlOutputFileName) throws Exception;

    public void exportASCIIFiles( String xmlSource, String exportFileName, Integer rootNodeNumber, HashSet allClimateFiles,HashSet allShortTermFiles,HashSet allLongTermFiles ) throws Exception;
    //    public void exportASCIIFiles( String xmlSource, String target, Integer rootNode, HashSet allClimateFiles,HashSet allShortTermFiles ) throws Exception;
	
    public Hashtable importResulsts( File resSource, File resTarget,String exportPattern,String dataSeparator ) throws Exception;
}

