/**
 * 
 */
package org.kalypso.afgui.model.impl;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.kalypso.afgui.model.ISubTaskGroup;
import org.kalypso.afgui.model.ISubTaskGroupSeq;

import com.hp.hpl.jena.rdf.model.NodeIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Seq;

/**
 * @author Patrice Congo
 *
 */
public class SubTaskGroupSeq implements ISubTaskGroupSeq
{
	private Seq seq;
	
	public SubTaskGroupSeq(Seq seq)
	{
		if(seq==null)
		{
			throw new IllegalArgumentException();
		}
		this.seq=seq;
	}
	
	public boolean add(ISubTaskGroup o)
	{
		if(o==null)
		{
			throw new NullPointerException();
		}
		Resource resource=(Resource)o.getModelObject();
		if(resource==null)
		{
			throw new IllegalArgumentException();
		}
		else
		{
			seq.add(resource);
			return true;
		}
		
	}

	public void add(int index, ISubTaskGroup element)
	{
		assertIndexInBounds(index, size());
		assertNotNull(element);
		
		Resource res=(Resource)element.getModelObject();
		if(res==null)
		{
			throw new IllegalArgumentException();
		}
		else
		{
			set(toSeqIndex(index), element);
		}
		
	}

	final static private int toSeqIndex(int index)
	{
		return index+1;
	}
	final static private void assertIndexInBounds(int index, int size)
	{
		if(index>=size || index<0)
		{
			throw new ArrayIndexOutOfBoundsException();
		}
		
	}

	final static private void assertNotNull(Object o)
	{
		if(o==null)
		{
			throw new NullPointerException();
		}
	}
	
	public boolean addAll(Collection<? extends ISubTaskGroup> c)
	{
		throw new UnsupportedOperationException();
	}

	public boolean addAll(int index, Collection<? extends ISubTaskGroup> c)
	{
		throw new UnsupportedOperationException();
	}

	public void clear()
	{
		for(int i=size();i>0;i--)
		{
			seq.remove(i);
		}
		
	}

	public boolean contains(Object o)
	{
		if(o instanceof ISubTaskGroup)
		{
			try
			{
				return seq.contains((Resource)((ISubTaskGroup)o).getModelObject());
			}
			catch(Throwable th)
			{
				return false;
			}
		}
		else
		{
			return false;
		}
	}

	public boolean containsAll(Collection<?> c)
	{
		throw new UnsupportedOperationException();
	}

	public ISubTaskGroup get(int index)
	{
		assertIndexInBounds(index, size());
		return new SubTaskGroup(
				seq.getResource(toSeqIndex(index)));
	}

	public int indexOf(Object o)
	{
		throw new UnsupportedOperationException();
	}

	public boolean isEmpty()
	{
		return size()==0;
	}

	public Iterator<ISubTaskGroup> iterator()
	{
		return new Iterator<ISubTaskGroup>()
		{
			private NodeIterator nit=seq.iterator();
			
			public boolean hasNext()
			{
				return nit.hasNext();
			}

			public ISubTaskGroup next()
			{
				return new SubTaskGroup((Resource)nit.nextNode());
			}

			public void remove()
			{
				throw new UnsupportedOperationException();
				
			}
			
		};
	}

	public int lastIndexOf(Object o)
	{
		throw new UnsupportedOperationException();
	}

	public ListIterator<ISubTaskGroup> listIterator()
	{
		throw new UnsupportedOperationException();
	}

	public ListIterator<ISubTaskGroup> listIterator(int index)
	{
		throw new UnsupportedOperationException();
	}

	public boolean remove(Object o)
	{
		assertNotNull(o);
		if(o instanceof ISubTaskGroup)
		{
			int i=seq.indexOf((Resource)((ISubTaskGroup)o).getModelObject());
			if(i==0)
			{
				return false;
			}
			else
			{
				seq.remove(i);
				return true;
			}
		}
		else
		{
			return false;
		}
	}

	public ISubTaskGroup remove(int index)
	{
		assertIndexInBounds(index, size());
		SubTaskGroup g= new SubTaskGroup(seq.getResource(toSeqIndex(index)));
		seq.remove(toSeqIndex(index));
		return g;
	}

	public boolean removeAll(Collection<?> c)
	{
		throw new UnsupportedOperationException();
	}

	public boolean retainAll(Collection<?> c)
	{
		throw new UnsupportedOperationException();
	}

	public ISubTaskGroup set(int index, ISubTaskGroup element)
	{
		assertIndexInBounds(index, size());
		assertNotNull(element);
		ISubTaskGroup g=get(index);
		seq.set(index, (Resource)element.getModelObject());
		return g;
	}

	public int size()
	{
		return seq.size();
	}

	public List<ISubTaskGroup> subList(int fromIndex, int toIndex)
	{
		throw new UnsupportedOperationException();
	}

	public Object[] toArray()
	{
		int size=size();
		Object[] array= new Object[size];
		toArray(array);
		return array;
	}

	public <T> T[] toArray(T[] a)
	{
		if(a==null)
		{
			throw new NullPointerException();
		}
		else
		{
			if(size()>a.length)
			{
				throw new IllegalArgumentException();
			}
			
			if(	a instanceof ISubTaskGroup[]||
				a instanceof Object[])
			{
				
				for(int i=0;i<a.length;i++)
				{
					a[i]=(T)get(i);
				}
				return a;
			}
			else
			{
				throw new UnsupportedOperationException();
			}
		}
	}
	
	@Override
	public boolean equals(Object obj)
	{
		//DOTO better equals implementation
		if(obj instanceof SubTaskGroupSeq)
		{
			return seq.equals(((SubTaskGroupSeq)obj).seq);
		}
		else
		{
			return false;
		}
	}
	
}
