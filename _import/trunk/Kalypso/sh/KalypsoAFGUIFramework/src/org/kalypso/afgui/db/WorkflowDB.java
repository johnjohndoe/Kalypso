package org.kalypso.afgui.db;


import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Platform;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;

public class WorkflowDB implements IWorkflowDB
{
	
	final static private Logger logger = Logger.getLogger( WorkflowDB.class.getName() );

	private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

    static
    {
      if( !log )
        logger.setUseParentHandlers( false );
    }

  // final private Map< String, IWorkflowDataCreationMechanism>
  // creationMechanism= new Hashtable<String, IWorkflowDataCreationMechanism>();
	
	
	private List<IWorkflowDBChangeListerner> dbListener=
		new ArrayList<IWorkflowDBChangeListerner>();
	final private Model dbModel;
	final private IResource dbDescResource;
	final private URL dbDescURL;
	
	public WorkflowDB(IResource dbDescRes) throws IOException
	{
		dbDescURL=dbDescRes.getRawLocationURI().toURL();
		dbModel=loadModel(dbDescURL);
		this.dbDescResource=dbDescRes;
	}
	
	final static private Model loadModel(URL url) throws IOException
	{
		// TODO: close input stream!! (finally!)
		logger.info("specURL="+url);
		InputStream iStream=url.openStream();
		Model rdfModel= ModelFactory.createDefaultModel();
		rdfModel.read(iStream,"");
		rdfModel.setNsPrefix(Schema.PJT_NS, url.toString()+"#");
		//rdfModel.write(System.out);
		return rdfModel;
	}
	
	
	
	public IWorkflowData createWorkflowData(
										String id, 
										String type,
										IWorkflowData parent)
	{
		
		logger.info("creating data for :"+id);		
		if(type==null)
		{
			throw new IllegalArgumentException(
					"Arguments type must not be null: id="+
					id+" type="+type+" parent="+parent);
		}
//		IWorkflowDataCreationMechanism creator=
//								creationMechanism.get(type);
//		logger.info("creator="+creator);
		//return creator.create(dbModel, id, type, parent);
		try
		{
			IWorkflowData  data= Schema.createWorkflowData(dbModel, parent, id);
			//logger.info("\n======================rdfModel\n"+dbModel);
			fireWorkflowDBChange();
			return data;
		}
		catch(Throwable th)
		{
			logger.log(Level.SEVERE, "error createing:" + id + " dbModel="
                    + dbModel, th);
            return null;
		}
		
	}

	public IWorkflowData derivedWorkflowData(
									IWorkflowData parent, 
									String childId)
	{
		
		return Schema.derivedWorkflowData(dbModel, parent, childId);
	}

	public IWorkflowData getWorkflowDataById(String id)
	{
		return Schema.getWorkflowDataById(dbModel, id);
	}

	public List<IWorkflowData> getWorkflowDataByType(String type)
	{
		return Schema.getWorkflowDataByType(dbModel, type);
	}
	
	

	public void link(
				IWorkflowData subject, 
				IWorkflowData object, 
				EWorkflowProperty prop)
	{
		Schema.createStatement(dbModel, subject, object, prop);
	}

	public void unlink(
					IWorkflowData subject, 
					IWorkflowData object, 
					EWorkflowProperty prop)
	{
		Schema.removeStatement(dbModel, subject, object, prop);
	}

	public List<IWorkflowData> getUnresolvable()
	{
		return null;
	}
	
	public List<IWorkflowData> getRootWorkflowDataByType(String type)
	{
		return Schema.getRootWorkflowDataByType(dbModel, type);
	}
	
	
	public void addWorkflowDBChangeListener(IWorkflowDBChangeListerner l)
	{
		logger.info("Registering:"+l);
		
		if(l==null)
		{
			return;
		}
		else
		{
			if(dbListener.contains(l))
			{
				return;
			}
			else
			{
				dbListener.add(l);
			}
		}
		
	}
	
	public void removeWorkflowDBChangeListener(IWorkflowDBChangeListerner l)
	{
		if(l==null)
		{
			return;
		}
		else
		{
			if(dbListener.contains(l))
			{
				dbListener.remove(l);
			}
		}
	}
	
	public void removeAllWorkflowDBChangeListener()
	{
		dbListener.clear();
		
	}
	
	private void fireWorkflowDBChange()
	{
		for(IWorkflowDBChangeListerner l:dbListener)
		{
			l.workflowDBChanged();
		}
	}
	
	public boolean persist()
	{
		try
		{
			
			logger.info("Persisting in :"+dbDescResource);
			FileOutputStream outStream=
						new FileOutputStream(dbDescResource.getLocationURI().toURL().getFile());
			dbModel.shortForm("RDF/XML-ABBREV");
			dbModel.write(outStream);
//			RDFWriter rdfWriter=dbModel.getWriter("RDF/XML-ABBREV");
//			rdfWriter.setProperty("relativeURIs", "same-document");
//			rdfWriter.write(dbModel, outStream, null);
			return true;
		}
		catch (Exception e)
		{
			logger.log(Level.SEVERE, "Could not save model", e);
			return false;
		}
	}
}
