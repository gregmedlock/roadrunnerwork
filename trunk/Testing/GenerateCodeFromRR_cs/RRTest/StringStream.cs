using System;

public sealed class StringStream
{
	private readonly Encoding _encoding;

	private readonly MemoryStream _stream;

	public StringStream(Encoding encoding, int capacity = 0)
	{
		_encoding = encoding;
		_stream = new MemoryStream(capacity);
	}

	public int Length
	{
		get { return (int)_stream.Length; }
	}

	public int Position
	{
		get { return (int)_stream.Position; }
		set { _stream.Position = value; }
	}

	public void Write(object obj)
	{
		var str = obj.ToString();
		var bytes = _encoding.GetBytes(str);
		_stream.Write(bytes, Position, bytes.Length);
		Position += bytes.Length;
	}

	public string ReadAll()
	{
		var bytes = new byte[Length];
		_stream.Read(bytes, 0, Length);
		Position = Length;
		return _encoding.GetString(bytes);
	}

	public string Read(int length)
	{
		var bytes = new byte[length];
		_stream.Read(bytes, Position, length);
		Position += length;
		return _encoding.GetString(bytes);
	}

	public T Read<T>()
	{
		var type = typeof(T);
		var size = Marshal.SizeOf(type);
		var bytes = new byte[size];
		_stream.Read(bytes, Position, size);
		Position += size;

		var handle = GCHandle.Alloc(bytes, GCHandleType.Pinned);
		var result = (T)Marshal.PtrToStructure(handle.AddrOfPinnedObject(), type);
		handle.Free();
		return result;
	}
}
