/*
 * Created on Oct 6, 2004
 *
  */
package org.kalypso.convert.namodel;

import java.io.IOException;
import java.io.Writer;
import java.net.URL;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.GMLWorkspace;

/**
 * @author doemming
 *
  */
public class ModelManager extends AbstractManager {

	/* (non-Javadoc)
	 * @see org.kalypso.convert.AbstractManager#mapID(int, org.deegree.model.feature.FeatureType)
	 */
	public ModelManager() throws IOException 
	{
		super(null);
	}
	
	public String mapID(int id, FeatureType ft)
	{
		return ft.getName()+id;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.convert.AbstractManager#parseFile(java.io.File)
	 */
	public Feature[] parseFile(URL url)  
	{
		// nothing to do
		return null;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.convert.AbstractManager#writeFile(java.io.Writer)
	 */
	public void writeFile(Writer writer,GMLWorkspace workspace)
	{
	// nothing to do
	}

}
